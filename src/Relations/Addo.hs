module Relations.Addo where

import MiniKanren
import Types.Nat
import Control.Applicative

addoRel :: (MiniKanren rel var) => Logic Nat var -> Logic Nat var -> Logic Nat var -> Relation rel ()
addoRel = relation3 "addo" $ \x y z -> asum
    [ do
        x === Z
        y <=> z
    , fresh2 $ \x' z' -> do
        x === S x'
        z === S z'
        call $ addoRel x' y z'
    ]
