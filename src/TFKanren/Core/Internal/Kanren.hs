{-# LANGUAGE GADTs, KindSignatures, FlexibleContexts, TypeFamilies #-}
module TFKanren.Core.Internal.Kanren(module TFKanren.Core.Internal.Kanren, Alternative(empty, (<|>)), EqVar(varEq)) where

import Data.Kind (Type)
import Control.Applicative (Alternative(empty, (<|>)))
import TFKanren.Core.Logic

data Relation (rel :: Type -> Type) = Relation String (rel ())

data FreshType (rel :: Type -> Type) (t :: (Type -> Type) -> Type) where
    FreshVar :: FreshType rel t
    ArgVar :: (Kanren rel, LogicVar t) => Logic t (KVar rel) -> FreshType rel t
data CallType = Opaque | Transparent


class (Alternative rel, Functor (KVar rel)) => Kanren rel where

    data KVar rel :: Type -> Type

    fresh_ :: (LogicVar t) => FreshType rel t -> (Var t (KVar rel) -> rel a) -> rel a

    unify :: (LogicVar a) => Logic a (KVar rel) -> Logic a (KVar rel) -> rel ()

    call_ :: CallType -> Relation rel -> rel ()

    displayVar :: KVar rel t -> String

class (Kanren rel) => KanrenEval rel where

    derefVar :: (Deref a g) => Var a (KVar rel) -> rel g

class EqVar rel where

    varEq :: (KVar rel) a -> (KVar rel) b -> Bool