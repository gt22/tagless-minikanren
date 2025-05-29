{-# LANGUAGE TypeFamilies, PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module TFKanren.Examples.Types.List(nil, cons, nil_, cons_, matchList_, pattern Nil, pattern Cons) where
import TFKanren.Utils.Type
import TFKanren.Core.Kanren

pattern Nil :: [a]
pattern Nil = []

pattern Cons :: a -> [a] -> [a]
pattern Cons h t = (h:t)

nil :: Logic [elem] var
nil = Ground Nil'

cons :: Logic elem var -> Logic [elem] var -> Logic [elem] var
cons h t = Ground $ Cons' h t

nil' :: WithLogic [elem] var
nil' = Nil'

cons' :: WithLogic elem var -> WithLogic [elem] var -> WithLogic [elem] var
cons' h t = Cons' (Ground h) (Ground t)

nil_ :: (Kanren rel, LogicType elem) => L [elem] rel -> rel a -> rel a
nil_ l r = l <=> nil *> r

cons_ :: (Kanren rel, LogicType elem) => L [elem] rel -> (L elem rel -> L [elem] rel -> rel a) -> rel a
cons_ l f = fresh2 $ \h t -> l <=> cons h t *> f h t

matchList_ :: (Kanren rel, LogicType elem) => L [elem] rel -> rel a -> (L elem rel -> L [elem] rel -> rel a) -> rel a
matchList_ l nil__ cons__ = conde [ nil_ l nil__, cons_ l cons__ ]

instance (LogicType a) => LogicType [a] where

    data WithLogic [a] var = Nil' | Cons' (Logic a var) (Logic [a] var)

    quote Nil' = quote0 "Nil" Nil'
    quote (Cons' h t) = quote2 "Cons" Cons' h t

    unifyVal _ Nil' Nil' = pure ()
    unifyVal unif (Cons' h t) (Cons' h' t') = unif h h' *> unif t t'
    unifyVal _ _ _ = empty

    derefVal _ Nil' = pure []
    derefVal deref (Cons' h t) = liftA2 (:) (deref h) (deref t)

    project [] = nil'
    project (x:xs) = cons' (project x) (project xs)

    reify Nil' = return []
    reify (Cons' (Ground h) (Ground t)) = liftA2 (:) (reify h) (reify t)
    reify (Cons' _ _) = Nothing

    generate = pure [] <|> (liftA2 (:) generate generate)
