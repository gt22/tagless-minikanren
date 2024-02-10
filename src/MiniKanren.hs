{-# LANGUAGE GADTs, FunctionalDependencies, FlexibleInstances, UndecidableInstances, EmptyDataDeriving #-}
module MiniKanren where

import Control.Applicative

class LogicVar var a where

    isVar :: a -> Maybe (var a)

class (MiniKanren rel var, LogicVar var a) => Unif rel var a | rel -> var where

    unifyVal :: a -> a -> rel ()

    unify :: a -> a -> rel ()
    unify a b = case isVar a of
        Nothing -> case isVar b of
            Nothing -> unifyVal a b
            Just b' -> unifyVar b' a
        Just a' -> unifyVar a' b

(===) :: (Unif action var a) => a -> a -> action ()
(===) = unify

class Fresh var a where

    makeFresh :: var a -> a

class (MiniKanrenEval rel var, LogicVar var a) => Deref rel var a g where

    derefVal :: a -> rel g

    deref :: a -> rel g
    deref a = case isVar a of
        Nothing -> derefVal a
        Just v -> do
            a' <- readVar v
            case a' of
                Nothing -> error "deref fail"
                Just av -> deref av

instance (MiniKanrenEval rel var, LogicVar var a) => Deref rel var a a where

    derefVal = return

class (Monad rel, Alternative rel) => MiniKanren rel var | rel -> var where

    freshVar :: rel (var a)

    fresh :: (Fresh var a) => (a -> rel s) -> rel s
    fresh f = freshVar >>= f . makeFresh

    unifyVar :: (Unif rel var a) => var a -> a -> rel ()

class (MiniKanren rel var) => MiniKanrenEval rel var where

    readVar :: var a -> rel (Maybe a)

fresh2 :: (MiniKanren rel var, Fresh var a, Fresh var b) => (a -> b -> rel s) -> rel s
fresh2 f = fresh $ \a -> fresh $ \b -> f a b

fresh3 :: (MiniKanren rel var, Fresh var a, Fresh var b,  Fresh var c) => (a -> b -> c -> rel s) -> rel s
fresh3 f = fresh $ \a -> fresh $ \b -> fresh $ \c -> f a b c

fresh4 :: (MiniKanren rel var, Fresh var a, Fresh var b,  Fresh var c, Fresh var d) => (a -> b -> c -> d -> rel s) -> rel s
fresh4 f = fresh $ \a -> fresh $ \b -> fresh $ \c -> fresh $ \d -> f a b c d 

fresh5 :: (MiniKanren rel var, Fresh var a, Fresh var b,  Fresh var c, Fresh var d, Fresh var e) => (a -> b -> c -> d -> e -> rel s) -> rel s
fresh5 f = fresh $ \a -> fresh $ \b -> fresh $ \c -> fresh $ \d -> fresh $ \e -> f a b c d e 


data NoVars a deriving (Show, Eq)
