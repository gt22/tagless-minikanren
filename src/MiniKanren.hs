{-# LANGUAGE Rank2Types, FunctionalDependencies, FlexibleInstances, EmptyDataDeriving, QuantifiedConstraints #-}
module MiniKanren where

import Control.Monad
import Control.Monad.Identity (Identity(runIdentity))
import Control.Applicative (empty)

type Var x var = var (Logic x var)

data Logic a var = Free (Var a var) | Ground (a var)

makeFresh :: Var a var -> Logic a var
makeFresh = Free

class LogicVar a where

    vmapMVal :: (Monad m) => (forall x. Var x var -> m (Var x var')) -> a var -> m (a var')

    showTerm :: (forall v. Show (Var v var)) => a var -> ShowS

vmapM :: (LogicVar a, Monad m) => (forall x. Var x var -> m (Var x var')) -> Logic a var -> m (Logic a var')
vmapM f (Free v) = makeFresh <$> f v
vmapM f (Ground x) = Ground <$> f `vmapMVal` x

vmapVal :: (LogicVar a) => (forall x. Var x var -> Var x var') -> a var -> a var'
vmapVal f x = runIdentity $ (return . f) `vmapMVal` x

vmap :: (LogicVar a) => (forall x. Var x var -> Var x var') -> Logic a var -> Logic a var'
vmap f x = runIdentity $ (return . f) `vmapM` x

showLogic :: (LogicVar a, forall v. Show (Var v var)) => Logic a var -> ShowS
showLogic (Free v) = shows v
showLogic (Ground x) = showTerm x

class (LogicVar a) => Unif a where

    unifyVal :: (MiniKanren rel var) => a var -> a var -> rel ()

unify :: (Unif a, MiniKanren rel var) => Logic a var -> Logic a var -> rel ()
unify (Free a) b = unifyVar a b
unify a (Free b) = unifyVar b a
unify (Ground a) (Ground b) = unifyVal a b

(===) :: (Unif a, MiniKanren rel var) => Logic a var -> a var -> rel ()
a === b = unify a (Ground b)

(<=>) :: (Unif a, MiniKanren rel var) => Logic a var -> Logic a var -> rel ()
a <=> b = unify a b

class LogicVar a => Deref a g where

    derefVal :: (MiniKanrenEval rel var) => a var -> rel g

class EqVar var where

    varEq :: var a -> var b -> Bool

class (MonadPlus rel) => MiniKanren rel var | rel -> var where

    freshVar :: rel (Var a var)
    unifyVar :: (Unif a) => Var a var -> Logic a var -> rel ()

unifyVar_ :: (Unif a, MiniKanrenEval rel var) => (Var a var -> rel (Maybe (Logic a var))) -> (Var a var -> Maybe (Logic a var) -> rel ()) -> Var a var -> Logic a var -> rel ()
unifyVar_ get set v a = do
        b' <- get v
        case b' of
            Nothing -> set v (Just a)
            Just b -> unify a b

class (MiniKanren rel var) => MiniKanrenEval rel var where

    readVar :: (Deref a g) => Var a var -> rel (Maybe g)
    

deref :: forall g a rel var. (Deref a g, MiniKanrenEval rel var) => Logic a var -> rel g
deref (Ground x) = derefVal x
deref (Free v) = do
    a' <- readVar v
    case a' of
        -- Nothing -> error "deref fail"
        Nothing -> empty
        Just av -> return av

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
