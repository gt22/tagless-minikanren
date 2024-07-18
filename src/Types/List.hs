{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, StandaloneDeriving, UndecidableInstances #-}
module Types.List where

import MiniKanren
import Control.Applicative

data List elem var = Nil | Cons (Logic elem var) (Logic (List elem) var)

deriving instance (Show (Logic elem var), Show (Logic (List elem) var)) => Show (List elem var)

nil :: Logic (List elem) var
nil = Ground Nil

cons :: Logic elem var -> Logic (List elem) var -> Logic (List elem) var
cons h t = Ground $ Cons h t

instance (LogicVar elem) => LogicVar (List elem) where

    _ `vmapMVal` Nil = return Nil
    f `vmapMVal` (Cons h t) = Cons <$> (f `vmapM` h) <*> (f `vmapM` t)

    showTerm Nil = showString "nil"
    showTerm (Cons h t) = showString "cons (" . showLogic h . showString ") (" . showLogic t . showString ")"

instance (Unif elem) => Unif (List elem) where

    unifyVal Nil Nil = return ()
    unifyVal (Cons h t) (Cons h' t') = unify h h' >> unify t t'
    unifyVal _ _ = empty


instance (Deref elem a) => Deref (List elem) [a] where

    derefVal Nil = return []
    derefVal (Cons h t) = (:) <$> deref h <*> deref t

instance (LogicVar elem, Deref elem (elem NoVars)) => Deref (List elem) (List elem NoVars) where

    derefVal Nil = return Nil
    derefVal (Cons h t) =  Cons <$> (Ground <$> deref h) <*> (Ground <$> deref t)