{-# LANGUAGE GADTs, FlexibleInstances, MultiParamTypeClasses, Rank2Types #-}
module Interpreters.Transformers.TreeMap where

import MiniKanren
import Interpreters.KanrenTree
import Control.Monad.State
import Control.Applicative
import Unsafe.Coerce (unsafeCoerce)
import Data.Foldable (traverse_)

newtype VarMapping s var = VarMapping {
    varSubst :: forall a. Var a (KanrenVar s) -> Var a var
    }
type KanrenMap s rel var = StateT (VarMapping s var) rel

setVar :: Var a (KanrenVar s) -> Var a var -> VarMapping s var -> VarMapping s var
setVar v u s = VarMapping $ \v' -> if v `varEq` v' then unsafeCoerce u else varSubst s v'

subst :: (Monad rel) => Var a (KanrenVar s) -> KanrenMap s rel var (Var a var)
subst v = do
    s <- get
    return $ varSubst s v

svmap :: (Monad rel, LogicVar a) => Logic a (KanrenVar s) -> KanrenMap s rel var (Logic a var)
svmap v = do
    s <- get
    return $ varSubst s `vmap` v

scoped :: (Monad m) => (s -> s) -> StateT s m a -> StateT s m a
scoped f act = gets f >>= lift . evalStateT act

evalKanren :: (MiniKanrenEval rel var) => Kanren s a -> KanrenMap s rel var a
evalKanren Fail = empty
evalKanren (Return a) = return a
evalKanren (Fresh v a) = do
    u <- lift freshVar
    scoped (setVar v u) (evalKanren a)
evalKanren (Arg arg v a) = do
    arg' <- svmap arg
    u <- lift $ argVar arg'
    scoped (setVar v u) (evalKanren a)
evalKanren (Deref t) = do
    t' <- subst t
    lift $ readVar t'
evalKanren (Unify v t) = do
    u <- subst v
    t' <- svmap t
    lift $ unifyVar u t'
evalKanren (Call n r) = do
    s <- get
    let r' = evalStateT (evalKanren r) s
    lift $ call (relation n r')
evalKanren (TransparentCall n r) = do
    s <- get
    let r' = evalStateT (evalKanren r) s
    lift $ call' (relation n r')
evalKanren (KConj xs) = traverse_ evalKanren xs
evalKanren (Conj a b) = evalKanren a >>= evalKanren . b
evalKanren (Disj a b) = evalKanren a <|> evalKanren b

runEvalKanren :: (MiniKanrenEval rel var) => (Kanren s a) -> rel a
runEvalKanren x = evalStateT (evalKanren x) (VarMapping $ \v -> error $ "Unknown variable: " ++ show v)