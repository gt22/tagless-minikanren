{-# LANGUAGE StandaloneDeriving, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Types.Bool where

import MiniKanren
import Control.Applicative

data Boolo var = VarN (var (Boolo var)) | Trueo | Falso  

deriving instance (Show (var (Boolo var))) => Show (Boolo var)

instance LogicVar Boolo where

    isVar (VarN v) = Just v
    isVar _ = Nothing

    f `vmapM` (VarN v) = VarN <$> f v
    _ `vmapM` Trueo = return Trueo
    _ `vmapM` Falso = return Falso

instance Unif Boolo where

    unifyVal Trueo Trueo = return ()
    unifyVal Falso Falso = return ()
    unifyVal _ _ = empty

instance Fresh Boolo where

    makeFresh :: var (Boolo var) -> Boolo var
    makeFresh = VarN

instance Deref Boolo Bool where

    derefVal Trueo = return True
    derefVal Falso = return False 
    derefVal _ = undefined

instance Deref Boolo (Boolo NoVars) where

    derefVal Trueo = return Trueo
    derefVal Falso = return Falso 
    derefVal _ = undefined