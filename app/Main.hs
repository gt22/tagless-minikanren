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
import Control.Monad (MonadPlus)
import Interpreters.TreeMap (runEvalKanren)
import Stream
import Stream (Delayable)
import Control.Monad.State (execStateT)

testSortoBacktr :: [[Int]]
testSortoBacktr = runLP $ run $ \xs -> sorto xs (cons zro (cons (suc zro) (cons (suc $ suc zro) nil)))

testAppendoBacktr :: [([Int], [Int])]
testAppendoBacktr = runLP $ run2 $ \xs ys -> appendo xs ys (cons zro nil)

testAddoBacktr :: [(Int, Int)]
testAddoBacktr = runLP $ run2 $ \x y -> addoRel x y (suc $ suc zro)

testAddoSubst :: (Delayable nondet) => nondet (Int, Int)
testAddoSubst = runSubstKanren $ run2 $ \x z -> addoRel x (suc $ suc zro) z

addoEmbed :: Kanren s (Int, Int)
addoEmbed = buildKanren $ run2 $ \x z -> addoRel x (suc $ suc zro) z

addoEmbedEval :: (MiniKanrenEval rel var) => rel (Int, Int)
addoEmbedEval = runEvalKanren addoEmbed

testAddoEmbedSubst :: (Delayable nondet) => nondet (Int, Int)
testAddoEmbedSubst = runSubstKanren addoEmbedEval

testAddoPrint :: String
testAddoPrint = printKanren $ runEvalKanren $ buildKanren $ fresh3 $ \x y z -> call' (addoRel x y z)

main :: IO ()
main = do
    print testSortoBacktr
    print testAppendoBacktr
    print testAddoBacktr
    print (takeS 5 testAddoSubst)
    print (takeS 5 testAddoEmbedSubst)
    putStrLn testAddoPrint
    
