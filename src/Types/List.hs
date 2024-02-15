{-# LANGUAGE StandaloneDeriving, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE InstanceSigs, FlexibleContexts #-}
module Types.List where

import MiniKanren
import Control.Applicative

data List elem var = VarL (var (List elem var)) | Nil | Cons (elem var) (List elem var) 

deriving instance (Show (elem var), Show (var (List elem var))) => Show (List elem var)

instance (LogicVar elem) => LogicVar (List elem) where

    makeFresh = VarL

    isVar (VarL v) = Just v
    isVar _ = Nothing

    _ `vmapMVal` Nil = return Nil
    f `vmapMVal` (Cons h t) = Cons <$> (f `vmapM` h) <*> (f `vmapM` t)
    _ `vmapMVal` _ = undefined

instance (Unif elem) => Unif (List elem) where

    unifyVal Nil Nil = return ()
    unifyVal (Cons h t) (Cons h' t') = unify h h' >> unify t t'
    unifyVal _ _ = empty

instance (Deref elem a) => Deref (List elem) [a] where

    derefVal Nil = return []
    derefVal (Cons h t) = (:) <$> deref h <*> deref t
    derefVal _ = undefined

instance (LogicVar elem, Deref elem (elem NoVars)) => Deref (List elem) (List elem NoVars) where

    derefVal Nil = return Nil
    derefVal (Cons h t) = Cons <$> deref h <*> deref t   
    derefVal _ = undefined