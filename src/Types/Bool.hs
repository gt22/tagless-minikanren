{-# LANGUAGE StandaloneDeriving, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, KindSignatures #-}
module Types.Bool where

import MiniKanren
import Control.Applicative
import Data.Kind (Type)

data Boolo (var :: Type -> Type) = Trueo | Falso  

deriving instance (Show (var (Boolo var))) => Show (Boolo var)

instance LogicVar Boolo where

    _ `vmapMVal` Trueo = return (Ground Trueo)
    _ `vmapMVal` Falso = return (Ground Falso)

instance Unif Boolo where

    unifyVal Trueo Trueo = return ()
    unifyVal Falso Falso = return ()
    unifyVal _ _ = empty

instance Deref Boolo Bool where

    derefVal Trueo = return True
    derefVal Falso = return False

instance Deref Boolo (Boolo NoVars) where

    derefVal Trueo = return Trueo
    derefVal Falso = return Falso
