{-# LANGUAGE Rank2Types, FlexibleInstances, MultiParamTypeClasses #-}
module Applicative.Interpreters.SubstKanren where

import Control.Monad
import Control.Applicative
import Applicative.MiniKanren
import MiniKanren (Logic(Free, Ground), Var, LogicVar, Relation(Relation), vmap)
import Control.Monad.State
import Unsafe.Coerce (unsafeCoerce)
import Stream (Delayable, immature)
import OptionT

newtype SVar s a = SVar Int deriving (Show, Eq)

instance EqVar (SVar s) where 
    varEq (SVar a) (SVar b) = a == b

newtype Subst s = Subst { subst :: forall a. SVar s a -> Maybe a }

emptySubst :: Subst s
emptySubst = Subst $ \v -> error $ "Invalid variable " ++ show v

readSubst :: SVar s a -> Subst s -> Maybe a
readSubst v s = subst s v

updateSubst :: SVar s a -> Maybe a -> Subst s -> Subst s
updateSubst v a (Subst s) = Subst $ \v' -> if v `varEq` v' then unsafeCoerce a else s v'

type Computation s nondet = StateT (Subst s) nondet

type KEval s nondet = StateT Int (Computation s nondet)

setVal :: (Monad nondet) => SVar s a -> Maybe a -> Computation s nondet ()
setVal v a = modify (updateSubst v a)

readVal :: (Monad nondet) => SVar s a -> Computation s nondet (Maybe a)
readVal v = gets (readSubst v)

instance (Delayable nondet) => Delayable (KEval s nondet) where
    
    immature = mapStateT (mapStateT immature)


makeFreshVar :: (MonadPlus nondet) => KEval s nondet (Var a (SVar s))
makeFreshVar = SVar <$> do
    v <- get
    modify succ
    return v

makeVar :: (MonadPlus nondet) => Maybe (Logic a (SVar s)) -> KEval s nondet (Var a (SVar s))
makeVar x = do
    v <- makeFreshVar
    lift $ setVal v x
    return v


instance (Delayable nondet) => MiniKanren (KEval s nondet) (SVar s) where

    fresh_ f = makeVar Nothing >>= f
    argument_ x f = makeVar (Just x) >>= f

    unifyVar = unifyVar_ (lift . readVal) (\v a -> lift $ setVal v a)

    call (Relation _ r) = immature r
    call' (Relation _ r) = r

instance (Delayable nondet) => MiniKanrenEval (KEval s nondet) (SVar s) where

    readVar v = do
        v' <- lift $ readVal v
        case v' of 
            Nothing -> return Nothing
            Just x -> Just <$> deref x

runSubstKanren :: (MonadPlus nondet) => (forall s. KEval s nondet a) -> nondet a
runSubstKanren k = evalStateT (evalStateT k 0) emptySubst