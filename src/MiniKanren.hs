{-# LANGUAGE GADTs, FunctionalDependencies, FlexibleInstances, UndecidableInstances, EmptyDataDeriving #-}
module MiniKanren where

import Control.Applicative
import Control.Monad

class LogicVar a where

    isVar :: a var -> Maybe (var (a var))

    vmapM :: (Monad m) => (var (a var) -> m (var' (a var'))) -> a var -> m (a var')

class (LogicVar a) => Unif a where

    unifyVal :: (MiniKanren rel var) => a var -> a var -> rel ()

    unify :: (MiniKanren rel var) => a var -> a var -> rel ()
    unify a b = case isVar a of
        Nothing -> case isVar b of
            Nothing -> unifyVal a b
            Just b' -> unifyVar b' a
        Just a' -> unifyVar a' b

(===) :: (Unif a, MiniKanren rel var) => a var -> a var -> rel ()
(===) = unify

class Fresh a where

    makeFresh :: var (a var) -> a var

class LogicVar a => Deref a g where

    derefVal :: (MiniKanrenEval rel var) => a var -> rel g

    deref :: (MiniKanrenEval rel var) => a var -> rel g
    deref a = case isVar a of
        Nothing -> derefVal a
        Just v -> do
            a' <- readVar v
            case a' of
                Nothing -> error "deref fail"
                Just av -> deref av

class EqVar var where

    varEq :: var a -> var b -> Bool

class (MonadPlus rel) => MiniKanren rel var | rel -> var where

    freshVar :: rel (var (a var))

    fresh :: (Fresh a) => (a var -> rel s) -> rel s
    fresh f = freshVar >>= f . makeFresh

    unifyVar :: (Unif a) => var (a var) -> a var -> rel ()

class (MiniKanren rel var) => MiniKanrenEval rel var where

    readVar :: (LogicVar a) => var (a var) -> rel (Maybe (a var))

fresh2 :: (MiniKanren rel var, Fresh a, Fresh b) => (a var -> b var -> rel s) -> rel s
fresh2 f = fresh $ \a -> fresh $ \b -> f a b

fresh3 :: (MiniKanren rel var, Fresh a, Fresh b,  Fresh c) => (a var -> b var -> c var -> rel s) -> rel s
fresh3 f = fresh $ \a -> fresh $ \b -> fresh $ \c -> f a b c

fresh4 :: (MiniKanren rel var, Fresh var a, Fresh var b,  Fresh var c, Fresh var d) => (a -> b -> c -> d -> rel s) -> rel s
fresh4 f = fresh $ \a -> fresh $ \b -> fresh $ \c -> fresh $ \d -> f a b c d 

fresh5 :: (MiniKanren rel var, Fresh var a, Fresh var b,  Fresh var c, Fresh var d, Fresh var e) => (a -> b -> c -> d -> e -> rel s) -> rel s
fresh5 f = fresh $ \a -> fresh $ \b -> fresh $ \c -> fresh $ \d -> fresh $ \e -> f a b c d e 


data NoVars a deriving (Show, Eq)
