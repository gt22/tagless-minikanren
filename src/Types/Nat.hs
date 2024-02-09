{-# LANGUAGE StandaloneDeriving, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
module Types.Nat where

import MiniKanren
import Control.Applicative

data Nat var = VarN (var (Nat var)) | Z | S (Nat var) 

deriving instance (Show (var (Nat var))) => Show (Nat var)

instance LogicVar var (Nat var) where

    isVar (VarN v) = Just v
    isVar _ = Nothing

instance (MiniKanren action var) => Unif action var (Nat var) where

    unifyVal Z Z = return ()
    unifyVal (S a) (S b) = unify a b
    unifyVal _ _ = empty

instance Fresh var (Nat var) where

    makeFresh = VarN

instance (MiniKanrenEval action var) => Deref action var (Nat var) Int where

    derefVal Z = return 0
    derefVal (S n) = succ <$> deref n
    derefVal _ = undefined

instance (MiniKanrenEval action var) => Deref action var (Nat var) (Nat NoVars) where

    derefVal Z = return Z
    derefVal (S n) = S <$> deref n
    derefVal _ = undefined