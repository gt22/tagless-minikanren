{-# LANGUAGE StandaloneDeriving, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
module Types.Nat where

import MiniKanren
import Control.Applicative

data Nat var = VarN (var (Nat var)) | Z | S (Nat var) 

deriving instance (Show (var (Nat var))) => Show (Nat var)

instance LogicVar Nat where

    makeFresh = VarN

    isVar (VarN v) = Just v
    isVar _ = Nothing

    _ `vmapMVal` Z = return Z
    f `vmapMVal` (S n) = S <$> (f `vmapM` n)
    _ `vmapMVal` _ = undefined

instance Unif Nat where

    unifyVal Z Z = return ()
    unifyVal (S a) (S b) = unify a b
    unifyVal _ _ = empty

instance Deref Nat Int where

    derefVal Z = return 0
    derefVal (S n) = succ <$> deref n
    derefVal _ = undefined

instance Deref Nat (Nat NoVars) where

    derefVal Z = return Z
    derefVal (S n) = S <$> deref n
    derefVal _ = undefined