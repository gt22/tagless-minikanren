{-# LANGUAGE StandaloneDeriving, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
module Types.Nat where

import MiniKanren
import Control.Applicative

data Nat var = VarN (var (Nat var)) | Z | S (Nat var) 

deriving instance (Show (var (Nat var))) => Show (Nat var)

instance LogicVar Nat where

    isVar (VarN v) = Just v
    isVar _ = Nothing

    f `vmapM` (VarN v) = VarN <$> f v
    _ `vmapM` Z = return Z
    f `vmapM` (S n) = S <$> (f `vmapM` n)

instance Unif Nat where

    unifyVal Z Z = return ()
    unifyVal (S a) (S b) = unify a b
    unifyVal _ _ = empty

instance Fresh Nat where

    makeFresh = VarN

instance Deref Nat Int where

    derefVal Z = return 0
    derefVal (S n) = succ <$> deref n
    derefVal _ = undefined

instance Deref Nat (Nat NoVars) where

    derefVal Z = return Z
    derefVal (S n) = S <$> deref n
    derefVal _ = undefined