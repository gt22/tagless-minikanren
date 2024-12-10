{-# LANGUAGE ApplicativeDo #-}
module Applicative.Relations.Addo where

import Applicative.MiniKanren
import MiniKanren (Logic, Relation)
import Types.Nat
import Control.Applicative

addoRel :: (MiniKanren rel var) => Logic Nat var -> Logic Nat var -> Logic Nat var -> Relation rel
addoRel = relation3 "addo" $ \x y z -> asum
    [ do
        x <=> zro
        y <=> z
        pure ()
    , fresh2 $ \x' z' -> do
        z <=> suc z'
        x <=> suc x'
        call $ addoRel x' y z'
        pure ()
    ]