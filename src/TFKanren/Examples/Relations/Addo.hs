{-# LANGUAGE ApplicativeDo #-}
module TFKanren.Examples.Relations.Addo(addo) where

import TFKanren.Examples.Types.Nat
import TFKanren.Core.Kanren

addo :: (Kanren rel) => L Nat rel -> L Nat rel -> L Nat rel -> Relation rel
addo = relation3 "addo" $ \x y z -> conde 
    [ do
        x <=> zro
        y <=> z
        pure ()
    , fresh2 $ \x' z' -> do
        x <=> suc x'
        call $ addo x' y z'
        z <=> suc z'
        pure ()
    ]