module Main (main) where

import TFKanren.Examples.Types.Nat
import TFKanren.Examples.Types.List
import TFKanren.Examples.Types.Tree
import TFKanren.Core.Logic
import TFKanren.Interpreters.SubstKanren.Stream
import TFKanren.Examples.Relations.Addo (addo)
import TFKanren.Core.Kanren
import TFKanren.Interpreters.SubstKanren.SubstKanren (runSubstKanren)
import TFKanren.Interpreters.Typeless.Typeless (Typeless, toTypeless, KVar(SynVar), freeArg)
import TFKanren.Examples.Relations.Balance
import qualified TFKanren.Interpreters.Normalization.Normalize as N

one = suc zro
two = suc one
thr = suc two

tr = cons zro $ cons one $ cons two $ cons thr nil

testTree = node (node (node leaf zro leaf) one leaf) two (node leaf thr leaf)

main :: IO ()
main = do
    -- print $ takeS 3 $ ((runSubstKanren $ run2 $ \x y -> addo x y (suc (suc zro))) :: Stream (Integer, Integer))
    -- print $ toTypeless $ addo (Free $ N.NV (SynVar 0)) (Free $ N.NV (SynVar 1)) (Free $ N.NV (SynVar 2))
    -- print $ toTypeless $ balancedo (Free $ N.NV (SynVar 0) :: L (Tree Nat) (N.NormalizedKanrenT Typeless))

    print $ toTypeless $ addo freeArg freeArg freeArg
    print $ toTypeless $ balancedo (freeArg :: L (Tree Nat) Typeless)
    -- print $ takeS 20 $ ((runSubstKanren $ run $ \mx -> maxo (suc $ suc zro) zro mx) :: Stream (Integer))