{-# LANGUAGE TypeApplications, LambdaCase, ApplicativeDo, MultiParamTypeClasses #-}
module TFKanren.Examples.Types.List(List(Nil, Cons), nil, cons) where
import TFKanren.Core.Internal.Logic
import TFKanren.Utils.Type

data List elem var = Nil | Cons (Logic elem var) (Logic (List elem) var)

nil :: Logic (List elem) var
nil = Ground Nil

cons :: Logic elem var -> Logic (List elem) var -> Logic (List elem) var
cons h t = Ground $ Cons h t

instance (LogicVar elem) => LogicVar (List elem) where

    quote Nil = quote0 "Nil" Nil
    quote (Cons h t) = quote2 "Cons" Cons h t

    unifyVal _ Nil Nil = pure ()
    unifyVal unif (Cons h t) (Cons h' t') = do
        unif h h'
        unif t t'
        pure ()
    unifyVal _ _ _ = empty

instance (Deref elem elemG) => Deref (List elem) [elemG] where 

    derefVal _ Nil = pure []
    derefVal deref (Cons h t) = liftA2 (:) (deref h) (deref t)