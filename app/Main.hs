{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
module Main (main) where


import MiniKanren(Logic(Free), Relation)
import Applicative.MiniKanren
import Interpreters.BacktrKanren
import Interpreters.KanrenTree
import Applicative.Interpreters.SubstKanren
import Interpreters.KanrenPrinter (printKanren)
import Types.Nat
import Types.List
import Applicative.Relations.Addo(addoRel)
import Relations.Appendo
import Relations.Sorto
import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Interpreters.Transformers.TreeMap (runEvalKanren)
import Stream
import Stream (Delayable)
import Control.Monad.State (execStateT)
import Interpreters.Transformers.PermuteConjuncts
import Applicative.Interpreters.Goal

-- testSortoBacktr :: [[Int]]
-- testSortoBacktr = runLP $ run $ \xs -> sorto xs (cons zro (cons (suc zro) (cons (suc $ suc zro) nil)))

-- testAppendoBacktr :: [([Int], [Int])]
-- testAppendoBacktr = runLP $ run2 $ \xs ys -> appendo xs ys (cons zro nil)

-- testAddoBacktr :: [(Int, Int)]
-- testAddoBacktr = runLP $ run2 $ \x y -> addoRel x y (suc $ suc zro)

testAddoSubst :: (Delayable nondet) => nondet (Int, Int)
testAddoSubst = runSubstKanren $ run2 $ \x z -> addoRel x (suc $ suc zro) z

addoGoal :: Term s Nat -> Term s Nat -> Relation (Goal s)
addoGoal x z = addoRel x (suc $ suc zro) z

testAddoGoalSubst :: (Delayable nondet) => nondet (Int, Int)
testAddoGoalSubst = runSubstKanren $ runGoal2' $ \x z -> call' $ addoGoal x z

testAddoReverseGoalSubst :: (Delayable nondet) => nondet (Int, Int)
testAddoReverseGoalSubst = runSubstKanren $ runGoal2' $ \x z -> reverseGoal $ call' $ addoGoal x z

-- testAddoSubst :: (Delayable nondet) => nondet (Int, Int)
-- testAddoSubst = runSubstKanren $ freshVar (\x -> freshVar (\z -> do
--     call (addoRel (Free x) (suc $ suc zro) (Free z))
--     x' <- deref (Free x)
--     z' <- deref (Free z)
--     return (x', z')
--     ))

-- addoEmbed :: Kanren s (Int, Int)
-- addoEmbed = buildKanren $ run2 $ \x z -> addoRel x (suc $ suc zro) z

-- addoEmbedEval :: (MiniKanrenEval rel var) => rel (Int, Int)
-- addoEmbedEval = runEvalKanren addoEmbed

-- testAddoEmbedSubst :: (Delayable nondet) => nondet (Int, Int)
-- testAddoEmbedSubst = runSubstKanren addoEmbedEval

-- testAddoPrint :: String
-- testAddoPrint = printKanren $ runEvalKanren $ buildKanren $ fresh3 $ \x y z -> call' (addoRel x y z)

-- testAddoPerm :: [String]
-- testAddoPerm = do
--     p <- producePermutations $ buildKanren $ fresh3 $ \x y z -> call' (addoRel x y z)
--     return $ printKanren $ runEvalKanren p

main :: IO ()
main = do
    -- print testSortoBacktr
    -- print testAppendoBacktr
    -- print testAddoBacktr
    print (takeS 5 testAddoSubst)
    print (takeS 5 testAddoGoalSubst)
    print (takeS 5 testAddoReverseGoalSubst)
    -- print (takeS 5 testAddoEmbedSubst)
    -- putStrLn testAddoPrint
    -- print testAddoPerm

