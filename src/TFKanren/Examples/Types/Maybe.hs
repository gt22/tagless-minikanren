{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances, TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module TFKanren.Examples.Types.Maybe(just, nothing) where
import TFKanren.Utils.Type
import TFKanren.Utils.Template.TypeTemplate

$(generateLogicType ''Maybe)

-- just :: Logic a var -> Logic (Maybe a) var
-- just a = Ground $ LogicJust a

-- nothing :: Logic (Maybe a) var
-- nothing = Ground $ LogicNothing

-- instance LogicType a => LogicType (Maybe a) where

--     data WithLogic (Maybe a) var = Just' (Logic a var) | Nothing'

--     quote (Just' a) = quote1 "Just" Just' a
--     quote Nothing' = quote0 "Nothing" Nothing'

--     unifyVal unif (Just' a) (Just' b) = unif a b
--     unifyVal _ Nothing' Nothing' = pure ()
--     unifyVal _ _ _ = empty

--     derefVal deref (Just' a) = Just <$> deref a
--     derefVal _ Nothing' = pure Nothing

--     project (Just a) = Just' (project' a)
--     project Nothing = Nothing'

--     reify (Just' a) = Just <$> reify' a
--     reify Nothing' = return Nothing

--     generate = pure Nothing <|> (Just <$> generate)

instance (Show (WithLogic a var), KanrenVar var) => Show (WithLogic (Maybe a) var) where

    show (LogicJust a) = "Just (" ++ showLogic a ++ ")"
    show LogicNothing = "Nothing"