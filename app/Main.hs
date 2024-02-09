module Main (main) where

import MiniKanren
import Interpreters.BacktrKanren
import Interpreters.KanrenTree
import Interpreters.SubstKanren
import Types.Nat
import Relations.Addo
import Control.Applicative (Alternative)

testAddoBacktr :: [(Int, Int)]
testAddoBacktr = runLP $ fresh2 $ \x y -> addo x y (S Z) >>= return (liftTuple (deref x, deref y))

testAddoSubst :: (Monad nondet, Alternative nondet) => nondet (Int, Int)
testAddoSubst = runSubstKanren $ fresh2 $ \x y -> addo x y (S Z) >>= return (liftTuple (deref x, deref y))

addoEmbed :: Kanren ()
addoEmbed = buildKanren $ fresh2 $ \x y -> addo x y (S Z)

main :: IO ()
main = do
    print testAddoBacktr
    print (testAddoSubst :: [(Int, Int)])
