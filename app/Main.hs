{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
module Main (main) where

import MiniKanren
import Interpreters.BacktrKanren
import Interpreters.KanrenTree
import Interpreters.SubstKanren
import Interpreters.KanrenPrinter (printKanren)
import Types.Nat
import Types.List
import Relations.Addo
import Relations.Appendo
import Relations.Sorto
import Control.Applicative (Alternative)
import Interpreters.TreeMap (runEvalKanren)

testAppendoBacktr :: [([Int], [Int])]
testAppendoBacktr = runLP $ fresh2 $ \xs ys -> appendo xs ys (Ground $ Cons zro (Ground Nil)) >>= return (liftTuple (deref xs, deref ys))

testSortoBacktr :: [[Int]]
testSortoBacktr = runLP $ fresh $ \xs -> sorto xs (Ground $ Cons zro (Ground $ Cons (suc zro) (Ground $ Cons (suc $ suc zro) (Ground Nil)))) >>= return (deref xs)

testAddoBacktr :: [(Int, Int)]
testAddoBacktr = runLP $ fresh2 $ \x y -> addo x y (suc zro) >>= return (liftTuple (deref x, deref y))

testAddoSubst :: (Monad nondet, Alternative nondet) => nondet (Int, Int)
testAddoSubst = runSubstKanren $ fresh2 $ \x y -> addo x y (suc zro) >>= return (liftTuple (deref x, deref y))

addoEmbed :: Kanren (Int, Int)
addoEmbed = buildKanren $ fresh2 $ \x y -> addo x y (suc zro) >>= return (liftTuple (deref x, deref y))

addoEmbedEval :: (MiniKanrenEval rel var) => rel (Int, Int)
addoEmbedEval = runEvalKanren addoEmbed

testAddoEmbedSubst :: (Monad nondet, Alternative nondet) => nondet (Int, Int)
testAddoEmbedSubst = runSubstKanren addoEmbedEval

testAddoPrint :: String
testAddoPrint = printKanren $ runEvalKanren $ buildKanren $ fresh2 $ \x y -> addoNoRec x y (suc zro) >>= return (liftTuple (deref @Int x, deref @Int y))

main :: IO ()
main = do
    print testSortoBacktr
    print testAppendoBacktr
    print testAddoBacktr
    print (testAddoSubst :: [(Int, Int)])
    print (testAddoEmbedSubst :: [(Int, Int)])
    putStrLn testAddoPrint
    
