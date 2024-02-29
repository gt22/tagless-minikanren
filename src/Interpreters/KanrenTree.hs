{-# LANGUAGE GADTs, FlexibleInstances, MultiParamTypeClasses #-}
module Interpreters.KanrenTree where

import MiniKanren
import Control.Monad.State
import Control.Applicative
import Control.Monad

data Kanren a where
    Fail :: Kanren a
    Return :: a -> Kanren a
    Deref :: (Deref x g) => Var x KanrenVar -> Kanren (Maybe g)
    Fresh :: Var x KanrenVar -> Kanren a -> Kanren a
    Unify :: (Unif a) => Var a KanrenVar -> (Logic a KanrenVar) -> Kanren ()

    Conj :: Kanren x -> (x -> Kanren a) -> Kanren a
    Disj :: Kanren a -> Kanren a -> Kanren a



newtype KanrenVar a = KVar Int deriving (Show, Eq)

instance EqVar KanrenVar where
    varEq (KVar a) (KVar b) = a == b

-- deriving instance (Show a) => Show (Kanren a)

instance Functor Kanren where

    fmap = liftA

instance Applicative Kanren where

    pure = Return

    (<*>) = ap

instance Monad Kanren where

    return = pure

    Fail >>= _ = Fail
    (Return a) >>= f = f a
    (Fresh v a) >>= f = Fresh v $ a >>= f
    (Disj a b) >>= f = Disj (a >>= f) (b >>= f)
    a >>= f = Conj a f

instance Alternative Kanren where

    empty = Fail

    (<|>) = Disj

instance MonadPlus Kanren

type KanrenAct = StateT Int Kanren

instance MiniKanren KanrenAct KanrenVar where

    freshVar = do
        v <- gets KVar
        modify succ
        lift $ Fresh v $ return v

    unifyVar v a = lift $ Unify v a

instance MiniKanrenEval KanrenAct KanrenVar where

    readVar = lift . Deref

buildKanren :: KanrenAct a -> Kanren a
buildKanren x = evalStateT x 0