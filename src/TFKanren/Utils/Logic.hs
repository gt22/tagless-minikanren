{-# LANGUAGE Rank2Types, EmptyDataDeriving #-}
module TFKanren.Utils.Logic
(
  NoVars
, vmapM, vmapM_
, vmapMVal, vmapMVal_
, vmap
) where
import TFKanren.Core.Internal.Logic
import Control.Monad.Identity (Identity(runIdentity, Identity))

data NoVars t deriving (Show, Eq)

vmapM :: (LogicVar a, Monad m) => (forall x. (LogicVar x) => Var x var -> m (Var x var')) -> Logic a var -> m (Logic a var')
vmapM f (Free v) = Free <$> f v
vmapM f (Ground x) = Ground <$> f `vmapMVal` x

vmapM_ :: (LogicVar a, Monad m) => (forall x. (LogicVar x) => Var x var -> m ()) -> Logic a var -> m ()
vmapM_ f (Free v) = f v
vmapM_ f (Ground x) = f `vmapMVal_` x

vmapMVal :: (LogicVar a, Monad m) => (forall x. (LogicVar x) => Var x var -> m (Var x var')) -> a var -> m (a var')
vmapMVal f x = let (con, elems) = quote x in reify con <$> (mapM (\(Field p x') -> Field p <$> f `vmapM` x') elems)

vmapMVal_ :: (LogicVar a, Monad m) => (forall x. (LogicVar x) => Var x var -> m ()) -> a var -> m ()
vmapMVal_ f x = let (_, elems) = quote x in (mapM_ (\(Field _ x') -> f `vmapM_` x') elems)

vmap :: (LogicVar a) => (forall x. (LogicVar x) => Var x var -> Var x var') -> Logic a var -> Logic a var'
vmap f x = runIdentity $ vmapM (Identity . f) x