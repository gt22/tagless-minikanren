{-# LANGUAGE GADTs, TypeFamilies, KindSignatures, QuantifiedConstraints, Rank2Types #-}
module TFKanren.Core.Internal.Logic(module TFKanren.Core.Internal.Logic) where
import Data.Kind (Type)
import Control.Applicative (Alternative)

data Field a var where
  Field :: LogicType x => Logic x var -> Field a var

data Constructor a = Constructor
  { name :: String
  , construct :: forall var. [Field a var] -> WithLogic a var
  }

class (Functor var, forall a. Show (var a)) => KanrenVar var
type Var x (var :: Type -> Type) = var (Logic x var)
data Logic a (var :: Type -> Type) 
  = Free (Var a var) | Ground (WithLogic a var)

class LogicType a where

  data WithLogic a (var :: Type -> Type) :: Type
  project :: a -> WithLogic a var
  reify :: WithLogic a var -> Maybe a
  
  quote :: WithLogic a var -> (Constructor a, [Field a var])
  
  unifyVal :: (Alternative rel) => (forall t. LogicType t => Logic t var -> Logic t var -> rel ()) -> WithLogic a var -> WithLogic a var -> rel ()
  derefVal :: (Alternative rel) => (forall t. LogicType t => Logic t var -> rel t) -> WithLogic a var -> rel a

  generate :: (Alternative list) => list a