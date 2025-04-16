{-# LANGUAGE TypeApplications, LambdaCase, MultiParamTypeClasses, FlexibleInstances #-}
module TFKanren.Examples.Types.Nat(Nat(Z, S), zro, suc) where
import TFKanren.Core.Logic
import TFKanren.Utils.Type

data Nat var = Z | S (Logic Nat var)

zro :: Logic Nat var
zro = Ground Z

suc :: Logic Nat var -> Logic Nat var
suc = Ground . S

instance LogicVar Nat where

    quote Z = quote0 "Z" Z
    quote (S n) = quote1 "S" S n

    unifyVal _ Z Z = pure ()
    unifyVal unif (S x) (S y) = unif x y
    unifyVal _ _ _ = empty

instance Deref Nat Integer where

    derefVal _ Z = pure 0
    derefVal deref (S n) = succ <$> (deref n)

instance Deref Nat (Nat NoVars) where

    derefVal _ Z = pure Z
    derefVal deref (S n) = S . Ground <$> (deref n)