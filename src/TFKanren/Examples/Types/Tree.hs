{-# LANGUAGE TypeApplications, LambdaCase, ApplicativeDo, MultiParamTypeClasses #-}
module TFKanren.Examples.Types.Tree(Tree, Tree', leaf, node) where

import TFKanren.Core.Internal.Logic
import TFKanren.Utils.Type
import Control.Applicative (liftA3)

data Tree elem var = Leaf | Node (Logic (Tree elem) var) (Logic elem var) (Logic (Tree elem) var)

data Tree' elem = Leaf' | Node' (Tree' elem) elem (Tree' elem)

instance (Show elem) => Show (Tree' elem) where

    showsPrec _ Leaf' = showString "()"
    showsPrec p (Node' l x r) = showString "(" . showsPrec p l . showString " - " . showsPrec p x . showString " - " . showsPrec p r . showString ")"

leaf :: Logic (Tree elem) var
leaf = Ground Leaf

node :: Logic (Tree elem) var -> Logic elem var -> Logic (Tree elem) var -> Logic (Tree elem) var
node l x r = Ground $ Node l x r

instance (LogicVar elem) => LogicVar (Tree elem) where

    quote Leaf = quote0 "Leaf" Leaf
    quote (Node l x r) = quote3 "Node" Node l x r

    unifyVal _ Leaf Leaf = pure ()
    unifyVal unif (Node l x r) (Node l' x' r') = do
        unif x x'
        unif l l'
        unif r r'
        pure ()
    unifyVal _ _ _ = empty

instance (Deref elem elemG) => Deref (Tree elem) (Tree' elemG) where

    derefVal _ Leaf = pure Leaf'
    derefVal deref (Node l x r) = liftA3 Node' (deref l) (deref x) (deref r)