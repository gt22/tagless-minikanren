{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Types.Nat where

import MiniKanren
import Control.Applicative

data Nat var = Z | S (Logic Nat var)

deriving instance (Show (Logic Nat var)) => Show (Nat var)

zro :: Logic Nat var
zro = Ground Z

suc :: Logic Nat var -> Logic Nat var
suc = Ground . S

instance LogicVar Nat where

    _ `vmapMVal` Z = return Z
    f `vmapMVal` (S n) = S <$> (f `vmapM` n)

    showTerm Z = showString "zro"
    showTerm (S n) = showString "suc " . showLogic n

instance Unif Nat where

    unifyVal Z Z = return ()
    unifyVal (S a) (S b) = unify a b
    unifyVal _ _ = empty

instance Deref Nat Int where

    derefVal Z = return 0
    derefVal (S n) = succ <$> deref n

instance Deref Nat (Nat NoVars) where

    derefVal Z = return Z
    derefVal (S n) = S <$> (Ground <$> deref n)