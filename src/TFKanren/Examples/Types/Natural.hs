{-# LANGUAGE TypeFamilies, FlexibleInstances, PatternSynonyms, LambdaCase, ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module TFKanren.Examples.Types.Natural(Natural, zro, suc, pattern Succ) where
import TFKanren.Utils.Type
import Numeric.Natural
pattern Succ :: Natural -> Natural
pattern Succ n <- ((`minusNaturalMaybe` 1) -> Just n)
  where Succ n = succ n 

zro :: Logic Natural var
zro = Ground Z

suc :: Logic Natural var -> Logic Natural var
suc = Ground . S

zro' :: WithLogic Natural var
zro' = Z

suc' :: WithLogic Natural var -> WithLogic Natural var
suc' = S . Ground

instance LogicType Natural where

    data WithLogic Natural var = Z | S (Logic Natural var)

    quote Z = quote0 "0" Z
    quote (S n) = quote1 "Succ" S n

    unifyVal _ Z Z = pure ()
    unifyVal unif (S x) (S y) = unif x y
    unifyVal _ _ _ = empty

    derefVal _ Z = pure 0
    derefVal deref (S n) = succ <$> (deref n)

    project 0 = zro'
    project n = suc' $ project (pred n)
    reify Z = return 0
    reify (S (Ground n)) = succ <$> reify n
    reify _ = Nothing

    generate = pure 0 <|> (succ <$> generate)


showsNat :: (KanrenVar var) => Natural -> WithLogic Natural var -> ShowS
showsNat m Z = shows m
showsNat m (S (Ground n)) = showsNat (succ m) n
showsNat m (S (Free v)) = showString "(" . shows m . showString " + " . shows v . showString ")"


instance (KanrenVar var) => Show (WithLogic Natural var) where
    showsPrec _ = showsNat 0