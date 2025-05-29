{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances, PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module TFKanren.Examples.Types.Pair(pair, pattern Pair') where

import TFKanren.Utils.Type

pattern Pair' :: a -> b -> (a, b)
pattern Pair' a b = (a, b)

pair :: Logic a var -> Logic b var -> Logic (a, b) var
pair a b = Ground $ Pair a b

instance (LogicType a, LogicType b) => LogicType (a, b) where

    data WithLogic (a, b) var = Pair (Logic a var) (Logic b var)

    quote (Pair a b) = quote2 "Pair'" Pair a b

    unifyVal unif (Pair a b) (Pair a' b') = unif a a' *> unif b b'

    derefVal deref (Pair a b) = liftA2 (,) (deref a) (deref b)

    project (a, b) = Pair (project' a) (project' b)

    reify (Pair a b) = liftA2 (,) (reify' a) (reify' b)

    generate = liftA2 (,) generate generate

instance (Show (WithLogic a var), Show (WithLogic b var), KanrenVar var) => Show (WithLogic (a, b) var) where
    show (Pair a b) = "Pair (" ++ showLogic a ++ ") (" ++ showLogic b ++ ")"