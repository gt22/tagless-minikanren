{-# LANGUAGE GADTs, KindSignatures, StandaloneDeriving, Rank2Types, UndecidableInstances, ApplicativeDo, QuantifiedConstraints #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TFKanren.Core.Internal.Logic(module TFKanren.Core.Internal.Logic) where
import Data.Kind (Type)
import Data.Proxy (Proxy)
import Control.Applicative

type Var x (var :: Type -> Type) = var (Logic x var)
data Logic (a :: (Type -> Type) -> Type) (var :: Type -> Type) 
  = Free (Var a var) | Ground (a var)

-- deriving instance (Show (Var a var), Show (a var)) => Show (Logic a var)


data Field a var where
  Field :: LogicVar x => Proxy x -> Logic x var -> Field a var

data Constructor a = Constructor
  { name :: String
  , reify :: forall var. [Field a var] -> a var
  }

class LogicVar (a :: (Type -> Type) -> Type) where

  quote :: a var -> (Constructor a, [Field a var])

  unifyVal :: (Alternative rel) => (forall t. LogicVar t => Logic t var -> Logic t var -> rel ()) -> a var -> a var -> rel ()

instance (LogicVar a, forall v. Show (Var v var)) => Show (Logic a var) where

  showsPrec p (Free v) = showsPrec p v
  showsPrec p (Ground t) = showsPrec p (LogicShow t)

instance (LogicVar a, forall v. Show (Var v var)) => Show (Field a var) where

  showsPrec p (Field _ x) = showsPrec p x

newtype LogicShow a (var :: Type -> Type) = LogicShow (a var)

instance (LogicVar a, forall v. Show (Var v var)) => Show (LogicShow a var) where

  showsPrec _ (LogicShow x) = let (con, xs) = quote x in showString (name con) . showString " " . showsPrec 11 xs


class (LogicVar a) => Deref a g where

  derefVal :: (Alternative rel) => (forall t g'. (Deref t g') => Logic t var -> rel g') -> a var -> rel g

-- LogicType Tree' where
--    data (Logic g) = Tree (Logic Tree) (Logic elem) (Logic Tree)
--    toLogic :: Tree' -> Logic Tree'
--    fromLogic :: Logic Tree' -> Tree'
--    unifyVal :: (LogicVar a) => (forall t. LogicVar t => Logic t var -> Logic t var -> rel ()) -> a var -> a var -> rel ()
--    quote :: (Logic Tree') var -> (Constructor Tree', [Field Tree' var])