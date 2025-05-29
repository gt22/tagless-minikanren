{-# LANGUAGE TypeFamilies, FlexibleInstances, PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module TFKanren.Examples.Types.Bool(true, false, pattern False_, pattern True_) where
import TFKanren.Utils.Type

pattern False_ :: Bool
pattern False_ = False

pattern True_ :: Bool
pattern True_ = True

true :: Logic Bool var
true = Ground $ True'

false :: Logic Bool var
false = Ground $ False'

instance LogicType Bool where

    data WithLogic Bool var = False' | True' deriving (Eq, Ord)

    quote False' = quote0 "False_" False'
    quote True' = quote0 "True_" True'

    unifyVal _ False' False' = pure ()
    unifyVal _ True' True' = pure ()
    unifyVal _ _ _ = empty

    derefVal _ False' = pure False
    derefVal _ True' = pure True

    project False = False'
    project True = True'

    reify False' = return False
    reify True' = return True

    generate = asum $ pure <$> [False, True]
    {-# INLINABLE generate #-}
    
instance Show (WithLogic Bool var) where
    show = show . reify