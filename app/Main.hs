module Main (main) where

import MiniKanren
import Interpreters.BacktrKanren
import Interpreters.KanrenTree
import Interpreters.SubstKanren
import Types.Nat
import Types.List 
import Relations.Addo
import Relations.Appendo
import Relations.Sorto
import Control.Applicative (Alternative)

testAppendoBacktr :: [([Int], [Int])]
testAppendoBacktr = runLP $ fresh2 $ \xs ys -> appendo xs ys (Ground $ Cons (Ground Z) (Ground Nil)) >>= return (liftTuple (deref xs, deref ys))

testSortoBacktr :: [[Int]]
testSortoBacktr = runLP $ fresh $ \xs -> sorto xs (Ground $ Cons (Ground Z) (Ground $ Cons (Ground $ S (Ground Z)) ((Ground $ Cons (Ground (S $ Ground (S $ Ground Z))) (Ground Nil))))) >>= return (deref xs)

testAddoBacktr :: [(Int, Int)]
testAddoBacktr = runLP $ fresh2 $ \x y -> addo x y (Ground $ S (Ground Z)) >>= return (liftTuple (deref x, deref y))

testAddoSubst :: (Monad nondet, Alternative nondet) => nondet (Int, Int)
testAddoSubst = runSubstKanren $ fresh2 $ \x y -> addo x y (Ground $ S (Ground Z)) >>= return (liftTuple (deref x, deref y))

addoEmbed :: Kanren ()
addoEmbed = buildKanren $ fresh2 $ \x y -> addo x y (Ground $ S (Ground Z))

main :: IO ()
main = do
    print testSortoBacktr
    print testAppendoBacktr
    print testAddoBacktr
    print (testAddoSubst :: [(Int, Int)])
