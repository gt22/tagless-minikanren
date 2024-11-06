{-# LANGUAGE GADTs, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
module Interpreters.KanrenTree where

import MiniKanren
import Control.Monad.State
import Control.Applicative
import Control.Monad

data Kanren s a where
    Fail :: Kanren s a
    Return :: a -> Kanren s a
    Deref :: (Deref x g) => Var x (KanrenVar s) -> Kanren s (Maybe g)
    Fresh :: Var x (KanrenVar s) -> Kanren s a -> Kanren s a
    Arg :: (LogicVar x) => Logic x (KanrenVar s) -> Var x (KanrenVar s) -> Kanren s a -> Kanren s a
    Unify :: (Unif a) => Var a (KanrenVar s) -> (Logic a (KanrenVar s)) -> Kanren s ()

    Call :: String -> Kanren s () -> Kanren s ()
    TransparentCall :: String -> Kanren s () -> Kanren s ()

    KConj :: [Kanren s ()] -> Kanren s ()
    Conj :: Kanren s x -> (x -> Kanren s a) -> Kanren s a
    Disj :: Kanren s a -> Kanren s a -> Kanren s a

newtype KanrenVar s a = KVar Int deriving (Show, Eq)

instance EqVar (KanrenVar s) where
    varEq (KVar a) (KVar b) = a == b

instance Functor (Kanren s) where

    fmap = liftA

instance Applicative (Kanren s) where

    pure = Return

    (<*>) = ap
-- a, b, fresh v in (c, d)
-- b, a, fresh v in (c, d)
-- a, b, fresh v in (d, c)
-- b, a, fresh v in (d, c)
-- a, fresh v in (c, d), b

-- argument $ \x -> Disj (KConj (fresh v in a) b) (c) >>= deref x
instance Monad (Kanren s) where

    return = pure

    Fail >>= _ = Fail
    (Return a) >>= f = f a
    (Fresh v a) >>= f = Fresh v $ a >>= f
    (Arg arg v a) >>= f = Arg arg v $ a >>= f
    -- (Disj a b) >>= f = Disj (a >>= f) (b >>= f)
    x@(KConj xs) >>= f = case f () of
        y@(Unify _ _) -> KConj (xs ++ [y])
        y@(Call _ _) -> KConj (xs ++ [y])
        y -> Conj x (const y)
    x@(Unify _ _) >>= f = case f () of
        y@(Unify _ _) -> KConj [x, y]
        y@(Call _ _) -> KConj [x, y]
        y -> Conj x (const y)
    x@(Call _ _) >>= f = case f () of
        y@(Unify _ _) -> KConj [x, y]
        y@(Call _ _) -> KConj [x, y]
        y -> Conj x (const y)
    a >>= f = Conj a f

instance Alternative (Kanren s) where

    empty = Fail

    (<|>) = Disj

instance MonadPlus (Kanren s)

type KanrenAct s = StateT Int (Kanren s)

makeFreshVar :: KanrenAct s (Var a (KanrenVar s))
makeFreshVar = KVar <$> do
    v <- get
    modify succ
    return v

instance MiniKanren (KanrenAct s) (KanrenVar s) where

    freshVar = do
        v <- makeFreshVar
        lift $ Fresh v $ return v

    argVar arg = do
        v <- makeFreshVar
        lift $ Arg arg v $ return v

    -- argVar arg = do
    --     v <- makeFreshVar
    --     lift $ Conj (Fresh v $ return v) (\v' -> Conj (Unify v' arg) (const $ return v'))

    unifyVar v a = lift $ Unify v a

    call (Relation n r) = get >>= lift . Call n . evalStateT r
    call' (Relation n r) = get >>= lift . TransparentCall n . evalStateT r

instance MiniKanrenEval (KanrenAct s) (KanrenVar s) where

    readVar = lift . Deref

buildKanren :: KanrenAct s a -> Kanren s a
buildKanren x = evalStateT x 0