{-# LANGUAGE GADTs #-}
module Interpreters.Transformers.PermuteConjuncts where
import MiniKanren
import Interpreters.Transformers.TreeMap
import Interpreters.KanrenTree
import Data.List (permutations)


producePermutations :: Kanren s a -> [Kanren s a]
producePermutations (KConj cs) = do
    topLevel <- permutations cs
    perms <- mapM producePermutations topLevel
    return $ KConj perms
producePermutations a = return a