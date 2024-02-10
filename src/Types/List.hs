{-# LANGUAGE StandaloneDeriving, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE InstanceSigs, FlexibleContexts #-}
module Types.List where

import MiniKanren
import Control.Applicative

data List var elem = VarL (var (List var elem)) | Nil | Cons elem (List var elem) 

deriving instance (Show elem, Show (var (List var elem))) => Show (List var elem)

instance LogicVar var (List var elem) where

    isVar (VarL v) = Just v
    isVar _ = Nothing

instance (MiniKanren action var, Unif action var elem) => Unif action var (List var elem) where

    unifyVal Nil Nil = return ()
    unifyVal (Cons h t) (Cons h' t') = unify h h' >> unify t t'
    unifyVal _ _ = empty

instance Fresh var (List var elem) where

    makeFresh :: var (List var elem) -> List var elem
    makeFresh = VarL

instance (MiniKanrenEval action var, Deref action var elem Int) => Deref action var (List var elem) [Int] where

    derefVal Nil = return []
    derefVal (Cons h t) = do
      h <- deref h 
      t <- deref t 
      return (h : t)
    derefVal _ = undefined

instance (MiniKanrenEval action var, LogicVar var elem) => Deref action var (List var elem) (List NoVars elem) where

    derefVal Nil = return Nil
    derefVal (Cons h t) = do 
      h <- deref h 
      t <- deref t 
      return (Cons h t)      
    derefVal _ = undefined