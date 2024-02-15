{-# LANGUAGE Rank2Types, FunctionalDependencies, FlexibleInstances, UndecidableInstances, EmptyDataDeriving #-}
module MiniKanren where

import Control.Monad

type Var x var = var (x var)

class LogicVar a where

    makeFresh :: Var a var -> a var

    isVar :: a var -> Maybe (Var a var)

    vmapMVal :: (Monad m) => (forall x. Var x var -> m (Var x var')) -> a var -> m (a var')

    vmapM :: (Monad m) => (forall x. Var x var -> m (Var x var')) -> a var -> m (a var')
    vmapM f x = case isVar x of
        Just v -> makeFresh <$> f v
        Nothing -> f `vmapMVal` x

class (LogicVar a) => Unif a where

    unifyVal :: (MiniKanren rel var) => a var -> a var -> rel ()

unify :: (Unif a, MiniKanren rel var) => a var -> a var -> rel ()
unify a b = case isVar a of
    Nothing -> case isVar b of
        Nothing -> unifyVal a b
        Just b' -> unifyVar b' a
    Just a' -> unifyVar a' b

(===) :: (Unif a, MiniKanren rel var) => a var -> a var -> rel ()
(===) = unify

class LogicVar a => Deref a g where

    derefVal :: (MiniKanrenEval rel var) => a var -> rel g

deref :: (MiniKanrenEval rel var, Deref a g) => a var -> rel g
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

    freshVar :: rel (Var a var)

    unifyVar :: (Unif a) => Var a var -> a var -> rel ()

unifyVar_ :: (Unif a, MiniKanrenEval rel var) => (Var a var -> Maybe (a var) -> rel ()) -> Var a var -> a var -> rel ()
unifyVar_ set v a = do
        b' <- readVar v
        case b' of
            Nothing -> set v (Just a)
            Just b -> unify a b

class (MiniKanren rel var) => MiniKanrenEval rel var where

    readVar :: (LogicVar a) => Var a var -> rel (Maybe (a var))

fresh :: (MiniKanren rel var, LogicVar a) => (a var -> rel s) -> rel s
fresh f = freshVar >>= f . makeFresh

fresh2 :: (MiniKanren rel var, LogicVar a, LogicVar b) => (a var -> b var -> rel s) -> rel s
fresh2 f = fresh $ \a -> fresh $ \b -> f a b

fresh3 :: (MiniKanren rel var, LogicVar a, LogicVar b,  LogicVar c) => (a var -> b var -> c var -> rel s) -> rel s
fresh3 f = fresh $ \a -> fresh $ \b -> fresh $ \c -> f a b c

fresh4 :: (MiniKanren rel var, LogicVar a, LogicVar b, LogicVar c, LogicVar d) => (a var -> b var -> c var -> d var -> rel s) -> rel s
fresh4 f = fresh $ \a -> fresh $ \b -> fresh $ \c -> fresh $ \d -> f a b c d 

fresh5 :: (MiniKanren rel var, LogicVar a, LogicVar b, LogicVar c, LogicVar d, LogicVar e) => (a var -> b var -> c var -> d var -> e var -> rel s) -> rel s
fresh5 f = fresh $ \a -> fresh $ \b -> fresh $ \c -> fresh $ \d -> fresh $ \e -> f a b c d e 


data NoVars a deriving (Show, Eq)
