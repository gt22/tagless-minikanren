{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module TFKanren.Examples.Types.Void(Void, absurd') where

import Data.Void
import TFKanren.Utils.Type

absurd' :: WithLogic Void var -> a
absurd' (RVoid x) = absurd x

instance LogicType Void where

    newtype WithLogic Void var = RVoid Void

    quote = absurd'
    unifyVal _ _ = absurd'
    derefVal _ = absurd'
    project = absurd
    reify = absurd'

    generate = empty