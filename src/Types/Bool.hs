{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, KindSignatures #-}
module Types.Bool where

import MiniKanren
import Control.Applicative
import Data.Kind (Type)

data Boolo (var :: Type -> Type) = Trueo | Falso

trueo :: Logic Boolo var
trueo = Ground Trueo
falso :: Logic Boolo var
falso = Ground Falso

instance LogicVar Boolo where

    _ `vmapMVal` Trueo = return Trueo
    _ `vmapMVal` Falso = return Falso

    showTerm Trueo = showString "trueo"
    showTerm Falso = showString "falso"

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
