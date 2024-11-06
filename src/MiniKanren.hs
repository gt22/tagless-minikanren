{-# LANGUAGE Rank2Types, FunctionalDependencies, FlexibleInstances, EmptyDataDeriving, QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
module MiniKanren where

import Control.Monad
import Control.Monad.Identity (Identity(runIdentity))
import Data.Kind (Type)

type Var x var = var (Logic x var)

data Logic a var = Free (Var a var) | Ground (a var)

deriving instance (Show (Var a var), Show (a var)) => Show (Logic a var)


data Relation (rel :: Type -> Type) = Relation String (rel ())


-- type family ValidRelation (rel :: Type -> Type) (built :: Type -> Type) (var :: Type -> Type) t :: Constraint where

--     ValidRelation rel built var (Relation rel built var t) = ()
--     ValidRelation rel built var (Argument rel built var x ret) = ValidRelation rel built var ret


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

    derefVal :: (MiniKanrenEval rel var, forall v. Show (Var v var)) => a var -> rel g

class EqVar var where

    varEq :: var a -> var b -> Bool

class (MonadPlus rel) => MiniKanren rel var | rel -> var where

    freshVar :: rel (Var a var)

    argVar :: (LogicVar a) => Logic a var -> rel (Var a var)

    unifyVar :: (Unif a) => Var a var -> Logic a var -> rel ()

    call :: Relation rel -> rel ()

    -- transparent call
    call' :: Relation rel -> rel ()
    call' = call


unifyVar_ :: (Unif a, MiniKanrenEval rel var) => (Var a var -> rel (Maybe (Logic a var))) -> (Var a var -> Maybe (Logic a var) -> rel ()) -> Var a var -> Logic a var -> rel ()
unifyVar_ get set v a = do
        b' <- get v
        case b' of
            Nothing -> set v (Just a)
            Just b -> unify a b

class (MiniKanren rel var, forall v. Show (Var v var)) => MiniKanrenEval rel var where

    readVar :: (Deref a g) => Var a var -> rel (Maybe g)


deref :: forall g a rel var. (Deref a g, MiniKanrenEval rel var) => Logic a var -> rel g
deref (Ground x) = derefVal x
deref (Free v) = do
    a' <- readVar v
    maybe (error $ "Unbound variable: " ++ show v) return a'
    -- maybe empty return a'

fresh :: (MiniKanren rel var) => (Logic a var -> rel s) -> rel s
fresh f = freshVar >>= f . Free

fresh2 :: (MiniKanren rel var) => (Logic a var -> Logic b var -> rel s) -> rel s
fresh2 f = fresh $ \a -> fresh $ \b -> f a b

fresh3 :: (MiniKanren rel var) => (Logic a var -> Logic b var -> Logic c var -> rel s) -> rel s
fresh3 f = fresh $ \a -> fresh $ \b -> fresh $ \c -> f a b c

fresh4 :: (MiniKanren rel var) => (Logic a var -> Logic b var -> Logic c var -> Logic d var -> rel s) -> rel s
fresh4 f = fresh $ \a -> fresh $ \b -> fresh $ \c -> fresh $ \d -> f a b c d

fresh5 :: (MiniKanren rel var) => (Logic a var -> Logic b var -> Logic c var -> Logic d var -> Logic e var -> rel s) -> rel s
fresh5 f = fresh $ \a -> fresh $ \b -> fresh $ \c -> fresh $ \d -> fresh $ \e -> f a b c d e

run :: (MiniKanrenEval rel var, Deref a a') => (Logic a var -> Relation rel) -> rel a'
run f = fresh $ \x -> do
    _ <- call' $ f x
    deref x

run2 :: (MiniKanrenEval rel var, Deref a a', Deref b b') => (Logic a var -> Logic b var -> Relation rel) -> rel (a', b')
run2 f = fresh2 $ \x y -> do
    _ <- call' $ f x y
    a <- deref x
    b <- deref y
    return (a, b)

run3 :: (MiniKanrenEval rel var, Deref a a', Deref b b', Deref c c') => (Logic a var -> Logic b var -> Logic c var -> Relation rel) -> rel (a', b', c')
run3 f = fresh3 $ \x y z -> do
    _ <- call' $ f x y z
    a <- deref x
    b <- deref y
    c <- deref z
    return (a, b, c)

run4 :: (MiniKanrenEval rel var, Deref a a', Deref b b', Deref c c', Deref d d') => (Logic a var -> Logic b var -> Logic c var -> Logic d var -> Relation rel) -> rel (a', b', c', d')
run4 f = fresh4 $ \x y z w -> do
    _ <- call' $ f x y z w
    a <- deref x
    b <- deref y
    c <- deref z
    d <- deref w
    return (a, b, c, d)

run5 :: (MiniKanrenEval rel var, Deref a a', Deref b b', Deref c c', Deref d d', Deref e e') => (Logic a var -> Logic b var -> Logic c var -> Logic d var -> Logic e var -> Relation rel) -> rel (a', b', c', d', e')
run5 f = fresh5 $ \x y z w q -> do
    _ <- call' $ f x y z w q
    a <- deref x
    b <- deref y
    c <- deref z
    d <- deref w
    e <- deref q
    return (a, b, c, d, e)

argument :: (MiniKanren rel var, LogicVar a) => Logic a var -> (Logic a var -> rel s) -> rel s
argument arg f = argVar arg >>= f . Free

argument2 :: (MiniKanren rel var, LogicVar a, LogicVar b) => Logic a var -> Logic b var -> (Logic a var -> Logic b var -> rel s) -> rel s
argument2 a_ b_ f = argument a_ $ \a -> argument b_ $ \b -> f a b

argument3 :: (MiniKanren rel var, LogicVar a, LogicVar b, LogicVar c) => Logic a var -> Logic b var -> Logic c var -> (Logic a var -> Logic b var -> Logic c var -> rel s) -> rel s
argument3 a_ b_ c_ f = argument a_ $ \a -> argument b_ $ \b -> argument c_ $ \c -> f a b c

argument4 :: (MiniKanren rel var, LogicVar a, LogicVar b, LogicVar c, LogicVar d) => Logic a var -> Logic b var -> Logic c var -> Logic d var -> (Logic a var -> Logic b var -> Logic c var -> Logic d var -> rel s) -> rel s
argument4 a_ b_ c_ d_ f = argument a_ $ \a -> argument b_ $ \b -> argument c_ $ \c -> argument d_ $ \d -> f a b c d

argument5 :: (MiniKanren rel var, LogicVar a, LogicVar b, LogicVar c, LogicVar d, LogicVar e) => Logic a var -> Logic b var -> Logic c var -> Logic d var -> Logic e var -> (Logic a var -> Logic b var -> Logic c var -> Logic d var -> Logic e var -> rel s) -> rel s
argument5 a_ b_ c_ d_ e_ f = argument a_ $ \a -> argument b_ $ \b -> argument c_ $ \c -> argument d_ $ \d -> argument e_ $ \e -> f a b c d e

relation :: String -> rel () -> Relation rel
relation = Relation

relation1 :: (MiniKanren rel var, LogicVar a) => String -> (Logic a var -> rel ()) -> Logic a var -> Relation rel
relation1 n f a_ = Relation n $ argument a_ f

relation2 :: (MiniKanren rel var, LogicVar a, LogicVar b) => String -> (Logic a var -> Logic b var -> rel ()) -> Logic a var -> Logic b var -> Relation rel
relation2 n f a_ b_ = Relation n $ argument2 a_ b_ f

relation3 :: (MiniKanren rel var, LogicVar a, LogicVar b, LogicVar c) => String -> (Logic a var -> Logic b var -> Logic c var -> rel ()) -> Logic a var -> Logic b var -> Logic c var -> Relation rel
relation3 n f a_ b_ c_ = Relation n $ argument3 a_ b_ c_ f

relation4 :: (MiniKanren rel var, LogicVar a, LogicVar b, LogicVar c, LogicVar d) => String -> (Logic a var -> Logic b var -> Logic c var -> Logic d var -> rel ()) -> Logic a var -> Logic b var -> Logic c var -> Logic d var -> Relation rel
relation4 n f a_ b_ c_ d_ = Relation n $ argument4 a_ b_ c_ d_ f

relation5 :: (MiniKanren rel var, LogicVar a, LogicVar b, LogicVar c, LogicVar d, LogicVar e) => String -> (Logic a var -> Logic b var -> Logic c var -> Logic d var -> Logic e var -> rel ()) -> Logic a var -> Logic b var -> Logic c var -> Logic d var -> Logic e var -> Relation rel
relation5 n f a_ b_ c_ d_ e_ = Relation n $ argument5 a_ b_ c_ d_ e_ f

data NoVars a deriving (Show, Eq)
