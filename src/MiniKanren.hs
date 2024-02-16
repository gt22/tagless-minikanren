{-# LANGUAGE Rank2Types, FunctionalDependencies, FlexibleInstances, UndecidableInstances, EmptyDataDeriving, StandaloneDeriving #-}
module MiniKanren where

import Control.Monad

type Var x var = var (Logic x var)

data Logic a var = Free (Var a var) | Ground (a var)

deriving instance (Show (Var a var), Show (a var)) => Show (Logic a var)

makeFresh :: Var a var -> Logic a var
makeFresh = Free

isVar :: Logic a var -> Either (Var a var) (a var)
isVar (Free v) = Left v
isVar (Ground x) = Right x

class LogicVar a where

    vmapMVal :: (Monad m) => (forall x. Var x var -> m (Var x var')) -> a var -> m (Logic a var')

    vmapM :: (Monad m) => (forall x. Var x var -> m (Var x var')) -> Logic a var -> m (Logic a var')
    vmapM f x = case isVar x of
        Left v -> makeFresh <$> f v
        Right x' -> f `vmapMVal` x'

class (LogicVar a) => Unif a where

    unifyVal :: (MiniKanren rel var) => a var -> a var -> rel ()

unify :: (Unif a, MiniKanren rel var) => Logic a var -> Logic a var -> rel ()
unify a b = case isVar a of
    Right a' -> case isVar b of
        Right b' -> unifyVal a' b'
        Left b' -> unifyVar b' a
    Left a' -> unifyVar a' b

(===) :: (Unif a, MiniKanren rel var) => Logic a var -> a var -> rel ()
a === b = unify a (Ground b)

(<=>) :: (Unif a, MiniKanren rel var) => Logic a var -> Logic a var -> rel ()
a <=> b = unify a b

class LogicVar a => Deref a g where

    derefVal :: (MiniKanrenEval rel var) => a var -> rel g

deref :: (MiniKanrenEval rel var, Deref a g) => Logic a var -> rel g
deref a = case isVar a of
    Right a' -> derefVal a'
    Left v -> do
        a' <- readVar v
        case a' of
            Nothing -> error "deref fail"
            Just av -> deref av

class EqVar var where

    varEq :: var a -> var b -> Bool

class (MonadPlus rel) => MiniKanren rel var | rel -> var where

    freshVar :: rel (Var a var)

    unifyVar :: (Unif a) => Var a var -> Logic a var -> rel ()

unifyVar_ :: (Unif a, MiniKanrenEval rel var) => (Var a var -> Maybe (Logic a var) -> rel ()) -> Var a var -> Logic a var -> rel ()
unifyVar_ set v a = do
        b' <- readVar v
        case b' of
            Nothing -> set v (Just a)
            Just b -> unify a b

class (MiniKanren rel var) => MiniKanrenEval rel var where

    readVar :: Var a var -> rel (Maybe (Logic a var))

fresh :: (MiniKanren rel var) => (Logic a var -> rel s) -> rel s
fresh f = freshVar >>= f . makeFresh

fresh2 :: (MiniKanren rel var) => (Logic a var -> Logic b var -> rel s) -> rel s
fresh2 f = fresh $ \a -> fresh $ \b -> f a b

fresh3 :: (MiniKanren rel var) => (Logic a var -> Logic b var -> Logic c var -> rel s) -> rel s
fresh3 f = fresh $ \a -> fresh $ \b -> fresh $ \c -> f a b c

fresh4 :: (MiniKanren rel var) => (Logic a var -> Logic b var -> Logic c var -> Logic d var -> rel s) -> rel s
fresh4 f = fresh $ \a -> fresh $ \b -> fresh $ \c -> fresh $ \d -> f a b c d 

fresh5 :: (MiniKanren rel var) => (Logic a var -> Logic b var -> Logic c var -> Logic d var -> Logic e var -> rel s) -> rel s
fresh5 f = fresh $ \a -> fresh $ \b -> fresh $ \c -> fresh $ \d -> fresh $ \e -> f a b c d e 


data NoVars a deriving (Show, Eq)
