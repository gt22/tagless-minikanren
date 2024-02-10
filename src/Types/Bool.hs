{-# LANGUAGE StandaloneDeriving, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Types.Bool where

import MiniKanren
import Control.Applicative

data Boolo var = VarN (var (Boolo var)) | Trueo | Falso  

deriving instance (Show (var (Boolo var))) => Show (Boolo var)

instance LogicVar var (Boolo var) where

    isVar (VarN v) = Just v
    isVar _ = Nothing

instance (MiniKanren action var) => Unif action var (Boolo var) where

    unifyVal Trueo Trueo = return ()
    unifyVal Falso Falso = return ()
    unifyVal _ _ = empty

instance Fresh var (Boolo var) where

    makeFresh :: var (Boolo var) -> Boolo var
    makeFresh = VarN

instance (MiniKanrenEval action var) => Deref action var (Boolo var) Bool where

    derefVal Trueo = return True
    derefVal Falso = return False 
    derefVal _ = undefined

instance (MiniKanrenEval action var) => Deref action var (Boolo var) (Boolo NoVars) where

    derefVal Trueo = return Trueo
    derefVal Falso = return Falso 
    derefVal _ = undefined