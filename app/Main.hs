{-# LANGUAGE TypeApplications, TemplateHaskell, KindSignatures, TypeFamilies, FlexibleContexts #-}
module Main (main) where

import TFKanren.Examples.Types.Natural
import TFKanren.Examples.Types.Unit
import TFKanren.Examples.Types.List
import TFKanren.Examples.Types.Tree
import TFKanren.Core.Logic
import TFKanren.Interpreters.SubstKanren.Stream
import TFKanren.Examples.Relations.Addo (addo)
import TFKanren.Examples.Relations.Arith
import TFKanren.Core.Kanren
import TFKanren.Interpreters.SubstKanren.SubstKanren (runSubstKanren)
import TFKanren.Interpreters.Typeless.Typeless (Typeless, toTypeless, toTypeless', KVar(SynVar), freeArg, namedArg)
import TFKanren.Examples.Relations.Balance
import qualified TFKanren.Interpreters.Normalization.Normalize as N
import TFKanren.Utils.Logic (vmap)
import Data.List (intercalate)
-- one = suc zro
-- two = suc one
-- thr = suc two

-- tr = cons zro $ cons one $ cons two $ cons thr nil

-- testTree = node (node (node leaf zro leaf) one leaf) two (node leaf thr leaf)

-- data TestType a = Z | O a | S [TestType a]

-- $(generateLogicType ''TestType)

test :: Maybe a -> [a]
test Nothing = []
test x = do
    (Just v) <- [x]
    return v

main :: IO ()
main = do
    print "hello"
    -- print $ takeS 3 $ ((runSubstKanren $ run2 $ \x y -> addo x y (suc (suc zro))) :: Stream (Integer, Integer))
    -- print $ toTypeless $ addo (Free $ N.NV (SynVar 0)) (Free $ N.NV (SynVar 1)) (Free $ N.NV (SynVar 2))
    -- print $ toTypeless $ balancedo (Free $ N.NV (SynVar 0) :: L (Tree Nat) (N.NormalizedKanrenT Typeless))

    -- print $ toTypeless $ addo freeArg freeArg freeArg
    -- print $ toTypeless $ balancedo (freeArg @(Tree ()))
    -- writeFile "balanceo.mk" $ intercalate "\n" $ map show $ map ((\v -> "x" ++ show v) <$>) (toTypeless $ balanceo (freeArg @(Tree ())) (freeArg @(Tree ())))
    -- print $ takeS 20 $ ((runSubstKanren $ embed $ lto zro (suc (suc zro))) :: Stream ())
    -- print $ takeS 20 $ ((runSubstKanren $ run $ \mx -> maxo (suc $ suc zro) zro mx) :: Stream Natural)
    -- print $ (takeS 20 $ ((runSubstKanren $ embed $ lto (Ground $ reify 0) (Ground $ reify 0)) :: Stream ())) == []
    -- print $ (takeS 20 $ ((runSubstKanren $ embed $ lto (Ground $ reify 2) (Ground $ reify 0)) :: Stream ())) == []
    -- print $ (takeS 20 $ ((runSubstKanren $ embed $ lto (Ground $ reify 0) (Ground $ reify 2)) :: Stream ())) == [()]
    -- print $ (takeS 20 $ ((runSubstKanren $ embed $ lto (Ground $ reify 2) (Ground $ reify 2)) :: Stream ())) == []
    -- print $ (takeS 20 $ ((runSubstKanren $ embed $ lto (Ground $ reify 1) (Ground $ reify 1)) :: Stream ())) == []
    -- print $ (takeS 20 $ ((runSubstKanren $ embed $ lto (Ground $ reify 2) (Ground $ reify 1)) :: Stream ())) == []
    -- print $ (takeS 20 $ ((runSubstKanren $ embed $ lto (Ground $ reify 1) (Ground $ reify 2)) :: Stream ())) == [()]
    -- print $ (takeS 20 $ ((runSubstKanren $ embed $ lto (Ground $ reify 2) (Ground $ reify 2)) :: Stream ())) == []

    -- print $ (takeS 20 $ ((runSubstKanren $ embed $ leo (Ground $ reify 0) (Ground $ reify 0)) :: Stream ())) == [()]
    -- print $ (takeS 20 $ ((runSubstKanren $ embed $ leo (Ground $ reify 2) (Ground $ reify 0)) :: Stream ())) == []
    -- print $ (takeS 20 $ ((runSubstKanren $ embed $ leo (Ground $ reify 0) (Ground $ reify 2)) :: Stream ())) == [()]
    -- print $ (takeS 20 $ ((runSubstKanren $ embed $ leo (Ground $ reify 2) (Ground $ reify 2)) :: Stream ())) == [()]
    -- print $ (takeS 20 $ ((runSubstKanren $ embed $ leo (Ground $ reify 1) (Ground $ reify 1)) :: Stream ())) == [()]
    -- print $ (takeS 20 $ ((runSubstKanren $ embed $ leo (Ground $ reify 2) (Ground $ reify 1)) :: Stream ())) == []
    -- print $ (takeS 20 $ ((runSubstKanren $ embed $ leo (Ground $ reify 1) (Ground $ reify 2)) :: Stream ())) == [()]
    -- print $ (takeS 20 $ ((runSubstKanren $ embed $ leo (Ground $ reify 2) (Ground $ reify 2)) :: Stream ())) == [()]