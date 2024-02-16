{-# LANGUAGE GADTs, FlexibleInstances, MultiParamTypeClasses, Rank2Types #-}
module Interpreters.KanrenTree where

import MiniKanren
import Control.Monad.State
import Control.Applicative
import Control.Monad

data Kanren a where
    Fail :: Kanren a
    Return :: a -> Kanren a
    Unify :: (Unif a) => Var a KanrenVar -> (Logic a KanrenVar) -> Kanren ()

    Conj :: Kanren x -> Kanren a -> Kanren a
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
    k@(Unify _ _) >>= f = Conj k (f ())
    (Conj a b) >>= f = Conj a (b >>= f)
    (Disj a b) >>= f = Disj (a >>= f) (b >>= f)

instance Alternative Kanren where

    empty = Fail

    (<|>) = Disj

instance MonadPlus Kanren

type KanrenAct = StateT Int Kanren

instance MiniKanren KanrenAct KanrenVar where

    freshVar = do
        v <- get
        modify succ
        return $ KVar v

    unifyVar v a = lift $ Unify v a

buildKanren :: KanrenAct a -> Kanren a
buildKanren x = evalStateT x 0