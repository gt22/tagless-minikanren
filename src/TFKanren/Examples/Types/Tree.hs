{-# LANGUAGE TypeFamilies, TemplateHaskell #-}
module TFKanren.Examples.Types.Tree(Tree(..), leaf, node, node_) where

import TFKanren.Utils.Type
import TFKanren.Core.Kanren
import Control.Applicative (liftA3)
import TFKanren.Utils.Template.TypeTemplate

data Tree elem = Leaf | Node (Tree elem) elem (Tree elem) deriving (Eq, Ord, Show)

$(generateLogicType ''Tree)

node_ :: (Kanren rel, LogicType elem) => L (Tree elem) rel -> (L (Tree elem) rel -> L elem rel -> L (Tree elem) rel -> rel a) -> rel a
node_ t f = fresh3 $ \l x r -> t <=> node l x r *> f l x r

-- leaf :: Logic (Tree elem) var
-- leaf = Ground Leaf'

-- node :: Logic (Tree elem) var -> Logic elem var -> Logic (Tree elem) var -> Logic (Tree elem) var
-- node l x r = Ground $ Node' l x r

-- leaf' :: WithLogic (Tree elem) var
-- leaf' = Leaf'

-- node' :: WithLogic (Tree elem) var -> WithLogic elem var -> WithLogic (Tree elem) var -> WithLogic (Tree elem) var
-- node' l x r = Node' (Ground l) (Ground x) (Ground r)

-- instance (LogicType a) => LogicType (Tree a) where

--     data WithLogic (Tree a) var = Leaf' | Node' (Logic (Tree a) var) (Logic a var) (Logic (Tree a) var)

--     quote Leaf' = quote0 "Leaf" Leaf'
--     quote (Node' l x r) = quote3 "Node" Node' l x r

--     unifyVal _ Leaf' Leaf' = pure ()
--     unifyVal unif (Node' l x r) (Node' l' x' r') = unif l l' *> unif x x' *> unif r r'
--     unifyVal _ _ _ = empty

--     derefVal _ Leaf' = pure Leaf
--     derefVal deref (Node' l x r) = liftA3 Node (deref l) (deref x) (deref r)

--     project Leaf = leaf'
--     project (Node l x r) = node' (project l) (project x) (project r)

--     reify Leaf' = return Leaf
--     reify (Node' (Ground l) (Ground x) (Ground r)) = liftA3 Node (reify l) (reify x) (reify r)
--     reify (Node' _ _ _) = Nothing

--     generate = pure Leaf <|> (liftA3 Node generate generate generate)