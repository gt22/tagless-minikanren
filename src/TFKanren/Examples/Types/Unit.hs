{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module TFKanren.Examples.Types.Unit(unit) where

import TFKanren.Utils.Type

unit :: Logic () var
unit = Ground Unit

instance LogicType () where

    data WithLogic () var = Unit

    quote Unit = quote0 "()" Unit

    unifyVal _ Unit Unit = pure ()

    derefVal _ Unit = pure ()
    project () = Unit
    reify Unit = return ()

    generate = pure ()