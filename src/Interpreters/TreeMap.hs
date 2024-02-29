{-# LANGUAGE GADTs, FlexibleInstances, MultiParamTypeClasses, Rank2Types #-}
module Interpreters.TreeMap where

import MiniKanren
import Interpreters.KanrenTree
import Control.Monad.State
import Control.Applicative
import Unsafe.Coerce (unsafeCoerce)

newtype VarMapping var = VarMapping {
    varSubst :: forall a. Var a KanrenVar -> Var a var
    }
type KanrenMap rel var = StateT (VarMapping var) rel

evalKanren :: (MiniKanrenEval rel var) => Kanren a -> KanrenMap rel var a
evalKanren Fail = empty
evalKanren (Return a) = return a
evalKanren (Fresh v a) = do
    u <- lift freshVar
    modify $ \s -> VarMapping $ \v' -> if v `varEq` v' then unsafeCoerce u else varSubst s v'
    evalKanren a
evalKanren (Deref t) = do
    s <- get
    let t' = varSubst s t
    lift $ readVar t'
evalKanren (Unify v t) = do
    s <- get
    let u = varSubst s v
    let t' = varSubst s `vmap` t
    lift $ unifyVar u t'
evalKanren (Conj a b) = evalKanren a >>= \x -> evalKanren (b x)
evalKanren (Disj a b) = evalKanren a <|> evalKanren b

runEvalKanren :: (MiniKanrenEval rel var) => Kanren a -> rel a
runEvalKanren x = evalStateT (evalKanren x) (VarMapping $ \v -> error $ "Unknown variable: " ++ show v)