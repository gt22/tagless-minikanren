{-# LANGUAGE Rank2Types, FunctionalDependencies, FlexibleInstances, EmptyDataDeriving, QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module V2.MiniKanren where

import Control.Monad
import Control.Monad.Identity (Identity(runIdentity))
import Data.Kind (Type)
import MiniKanren (Var, Logic(Free, Ground), Relation(Relation))
import Data.Maybe (fromMaybe)


class LogicVar a where

    vmapMVal :: (Monad m) => (forall x. Var x var -> m (Var x var')) -> a var -> m (a var')

    showTerm :: (forall v. Show (Var v var)) => a var -> ShowS

vmapM :: (LogicVar a, Monad m) => (forall x. Var x var -> m (Var x var')) -> Logic a var -> m (Logic a var')
vmapM f (Free v) = Free <$> f v
vmapM f (Ground x) = Ground <$> f `vmapMVal` x

vmapVal :: (LogicVar a) => (forall x. Var x var -> Var x var') -> a var -> a var'
vmapVal f x = runIdentity $ (return . f) `vmapMVal` x

vmap :: (LogicVar a) => (forall x. Var x var -> Var x var') -> Logic a var -> Logic a var'
vmap f x = runIdentity $ (return . f) `vmapM` x

showLogic :: (LogicVar a, forall v. Show (Var v var)) => Logic a var -> ShowS
showLogic (Free v) = shows v
showLogic (Ground x) = showTerm x


class (LogicVar a) => Unif a where

    unifyVal :: (MiniKanren rel disj conj core var) => a var -> a var -> core ()

unify :: (Unif a, MiniKanren rel disj conj core var) => Logic a var -> Logic a var -> core ()
unify (Free a) b = unifyVar a b
unify a (Free b) = unifyVar b a
unify (Ground a) (Ground b) = unifyVal a b

(===) :: (Unif a, MiniKanren rel disj conj core var) => Logic a var -> a var -> core ()
a === b = unify a (Ground b)

(<=>) :: (Unif a, MiniKanren rel disj conj core var) => Logic a var -> Logic a var -> core ()
a <=> b = unify a b

class (Monad rel) => MiniKanren rel disj conj core var
      | rel -> disj, disj -> conj, conj -> core, core -> rel, core -> var where

    unifyVar :: (Unif a) => Var a var -> Logic a var -> core ()

    call :: Relation rel -> core ()

    -- transparent call
    call' :: Relation rel -> core ()
    call' = call

    raiseToConj :: core () -> conj ()

    raiseToDisj :: conj () -> disj ()

    raiseToRel :: disj () -> rel ()

    freshVar :: (Var a var -> rel x) -> rel x

    argVar :: (LogicVar a) => Logic a var -> (Var a var -> rel x) -> rel x

    sucunif :: core ()
    nonunif :: core ()

    trivial :: conj ()
    failing :: disj ()
    conj :: core () -> conj () -> conj ()
    disj :: conj () -> disj () -> disj ()

    bigconj :: [core ()] -> conj ()
    bigconj = foldr conj trivial
    bigdisj :: [conj ()] -> disj ()
    bigdisj = foldr disj failing

    biggestdisj :: [[core ()]] -> disj ()
    biggestdisj = bigdisj . (bigconj <$>)

class (Functor rel, MiniKanren rel disj conj core var, forall v. Show (Var v var)) => MiniKanrenEval rel disj conj core var where

    readVar :: (Deref a g) => Var a var -> rel (Maybe g)

unifyVar_ :: (Unif a, Monad core, MiniKanren rel disj conj core var) => (Var a var -> core (Maybe (Logic a var))) -> (Var a var -> Maybe (Logic a var) -> core ()) -> Var a var -> Logic a var -> core ()
unifyVar_ get set v a = do
        b' <- get v
        case b' of
            Nothing -> set v (Just a)
            Just b -> unify a b

class LogicVar a => Deref a g where

    derefVal :: (MiniKanrenEval rel disj conj core var, forall v. Show (Var v var)) => a var -> rel g

deref :: forall g a rel disj conj core var. (Deref a g, MiniKanrenEval rel disj conj core var) => Logic a var -> rel g
deref (Ground x) = derefVal x
deref (Free v) = fromMaybe (error $ "Unbound variable: " ++ show v) <$> readVar v

fresh :: (MiniKanren rel disj conj core var) => (Logic a var -> rel s) -> rel s
fresh f = freshVar (f . Free)

fresh2 :: (MiniKanren rel disj conj core var) => (Logic a var -> Logic b var -> rel s) -> rel s
fresh2 f = fresh $ \a -> fresh $ \b -> f a b

argument :: (MiniKanren rel disj conj core var, LogicVar a) => Logic a var -> (Logic a var -> rel s) -> rel s
argument arg f = argVar arg (f . Free)

argument2 :: (MiniKanren rel disj conj core var, LogicVar a, LogicVar b) => Logic a var -> Logic b var -> (Logic a var -> Logic b var -> rel s) -> rel s
argument2 a_ b_ f = argument a_ $ \a -> argument b_ $ \b -> f a b

argument3 :: (MiniKanren rel disj conj core var, LogicVar a, LogicVar b, LogicVar c) => Logic a var -> Logic b var -> Logic c var -> (Logic a var -> Logic b var -> Logic c var -> rel s) -> rel s
argument3 a_ b_ c_ f = argument a_ $ \a -> argument b_ $ \b -> argument c_ $ \c -> f a b c

relation3 :: (MiniKanren rel disj conj core var, LogicVar a, LogicVar b, LogicVar c) => String -> (Logic a var -> Logic b var -> Logic c var -> rel ()) -> Logic a var -> Logic b var -> Logic c var -> Relation rel
relation3 n f a_ b_ c_ = Relation n $ argument3 a_ b_ c_ f