{-# LANGUAGE GADTs, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
module Applicative.Interpreters.KanrenTree where

import Applicative.MiniKanren
import MiniKanren (Logic(Free, Ground), Var, LogicVar, Relation(Relation), vmap)
import Control.Monad.State
import Control.Applicative
import Control.Monad
import Unsafe.Coerce (unsafeCoerce)

data Kanren s a where
    Fail :: Kanren s a
    Return :: a -> Kanren s a
    Deref :: (Deref x g) => Var x (KanrenVar s) -> Kanren s (Maybe g)
    Fresh :: (Var x (KanrenVar s) -> Kanren s a) -> Kanren s a
    Arg :: (LogicVar x) => Logic x (KanrenVar s) -> (Var x (KanrenVar s) -> Kanren s a) -> Kanren s a
    Unify :: (Unif a) => Var a (KanrenVar s) -> (Logic a (KanrenVar s)) -> Kanren s ()

    Call :: String -> Kanren s () -> Kanren s ()
    TransparentCall :: String -> Kanren s () -> Kanren s ()

    Conj :: Kanren s (a -> b) -> Kanren s a -> Kanren s b
    Disj :: Kanren s a -> Kanren s a -> Kanren s a

newtype KanrenVar s a = KVar Int deriving (Show, Eq)

instance EqVar (KanrenVar s) where
    varEq (KVar a) (KVar b) = a == b

instance Functor (Kanren s) where

    fmap = liftA

instance Applicative (Kanren s) where

    pure = Return

    (<*>) = Conj

instance Alternative (Kanren s) where

    empty = Fail

    (<|>) = Disj

instance MiniKanren (Kanren s) (KanrenVar s) where

    fresh_ = Fresh

    argument_ = Arg

    unifyVar = Unify

    call (Relation n r) = Call n r
    call' (Relation n r) = TransparentCall n r

instance MiniKanrenEval (Kanren s) (KanrenVar s) where

    readVar = Deref


data VarMapping s var = VarMapping {
    varSubst :: forall a. Var a (KanrenVar s) -> Var a var,
    varGen :: Int
    }
type KanrenMap s var a = State (VarMapping s var) a

-- do
--   x == y
--   y == 0
--   return ()

-- (Return $ \s1 s2 -> ()) <*> (x == y) <*> (y == 0)

setVar :: Var a (KanrenVar s) -> Var a var -> VarMapping s var -> VarMapping s var
setVar v u s = s { varSubst = \v' -> if v `varEq` v' then unsafeCoerce u else varSubst s v' }

makeVar :: KanrenMap s var (KanrenVar s a)
makeVar = do
    n <- gets varGen
    modify $ \s -> s { varGen = succ $ varGen s }
    return $ KVar n

subst :: Var a (KanrenVar s) -> KanrenMap s var (Var a var)
subst v = do
    s <- get
    return $ varSubst s v

svmap :: (LogicVar a) => Logic a (KanrenVar s) -> KanrenMap s var (Logic a var)
svmap v = do
    s <- get
    return $ varSubst s `vmap` v

scoped :: (Monad m) => (s -> s) -> StateT s m a -> StateT s m a
scoped f act = gets f >>= lift . evalStateT act

evalKanren :: (MiniKanrenEval rel var) => Kanren s a -> KanrenMap s var (rel a)
evalKanren Fail = return empty
evalKanren (Return a) = return $ pure a
evalKanren (Fresh f) = do
    v <- makeVar
    s <- get
    return $ fresh_ $ \v' -> evalState (modify (setVar v v') >> evalKanren (f v)) s
evalKanren (Arg arg f) = do
    arg' <- svmap arg
    v <- makeVar
    s <- get
    return $ argument_ arg' $ \v' -> evalState (modify (setVar v v') >> evalKanren (f v)) s
evalKanren (Deref t) = do
    t' <- subst t
    return $ readVar t'
evalKanren (Unify v t) = do
    u <- subst v
    t' <- svmap t
    return $ unifyVar u t'
evalKanren (Call n r) = do
    s <- get
    let r' = evalState (evalKanren r) s
    return $ call (relation n r')
evalKanren (TransparentCall n r) = do
    s <- get
    let r' = evalState (evalKanren r) s
    return $ call' (relation n r')
evalKanren (Conj a b) = do
    b' <- evalKanren b
    a' <- evalKanren a
    return $ a' <*> b'
evalKanren (Disj a b) = do
    a' <- evalKanren a
    b' <- evalKanren b
    return $ a' <|> b'

runEvalKanren :: (MiniKanrenEval rel var) => Kanren s a -> rel a
runEvalKanren x = evalState (evalKanren x) (VarMapping { varSubst = \v -> error $ "Unknown variable: " ++ show v, varGen = 0 })