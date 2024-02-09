{-# LANGUAGE Rank2Types, FlexibleInstances, MultiParamTypeClasses, DeriveFunctor #-}
module Interpreters.SubstKanren where

import Control.Monad
import Control.Applicative
import MiniKanren
import Control.Monad.State
import Unsafe.Coerce (unsafeCoerce)

newtype SVar s a = SVar Int deriving (Show, Eq)

varEq :: SVar s a -> SVar s b -> Bool
varEq (SVar a) (SVar b) = a == b

newtype Subst s = Subst { subst :: forall a. SVar s a -> Maybe a }

updateSubst :: Subst s -> SVar s a -> Maybe a -> Subst s
updateSubst (Subst s) v a = Subst $ \v' -> if v `varEq` v' then unsafeCoerce a else s v'

newtype KEval s nondet a = KEval { runK :: Subst s -> nondet (a, Subst s) } deriving (Functor)

setVal :: (Monad nondet) => SVar s a -> Maybe a -> KEval s nondet ()
setVal v a = KEval $ \s -> return ((), updateSubst s v a)

readVal :: (Monad nondet) => SVar s a -> KEval s nondet (Maybe a)
readVal v = KEval $ \s -> return (subst s v, s)

instance (Monad nondet) => Applicative (KEval s nondet) where

    pure a = KEval $ \s -> pure (a, s)

    (<*>) = ap

instance (Monad nondet) => Monad (KEval s nondet) where

    return = pure

    (KEval f) >>= g = KEval $ f >=> (\(a, s') -> runK (g a) s')

instance (Monad nondet, Alternative nondet) => Alternative (KEval s nondet) where

    empty = KEval $ const empty

    (KEval f) <|> (KEval g) = KEval $ \s -> f s <|> g s

instance (Monad nondet, Alternative nondet) => MonadPlus (KEval s nondet)

type KEvalAct s nondet = StateT Int (KEval s nondet)

instance (Monad nondet, Alternative nondet) => MiniKanren (KEvalAct s nondet) (SVar s) where

    freshVar = do
        x <- get
        modify succ
        let v = SVar x
        lift $ setVal v Nothing
        return $ SVar x

    unifyVar v a = do
        b' <- lift $ readVal v
        case b' of
            Nothing -> lift $ setVal v (Just a)
            Just b -> unify a b

instance (Monad nondet, Alternative nondet) => MiniKanrenEval (KEvalAct s nondet) (SVar s) where

    readVar = lift . readVal

runSubstKanren :: (Monad nondet) => (forall s. StateT Int (KEval s nondet) a) -> nondet a
runSubstKanren k = fst <$> runK (evalStateT k 0) (Subst $ \v -> error $ "Invalid variable " ++ show v)