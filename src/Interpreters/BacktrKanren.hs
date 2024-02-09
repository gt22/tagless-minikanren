{-# LANGUAGE Rank2Types, FlexibleInstances, MultiParamTypeClasses, DeriveFunctor #-}
module Interpreters.BacktrKanren where

import MiniKanren
import Control.Monad.ST
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Control.Applicative
import Control.Monad.Trans
import Control.Monad

newtype BacktrT m a = BTT { unBTT :: forall ans. (a -> m [ans]) -> m [ans] } deriving (Functor)

instance (Monad m) => Applicative (BacktrT m) where

    pure x = BTT $ \f -> f x

    (<*>) = ap

instance (Monad m) => Alternative (BacktrT m) where

    empty = BTT $ const (return [])

    (BTT a) <|> (BTT b) = BTT $ \c -> do
        av <- a c
        bv <- b c
        return $ av <|> bv

instance (Monad m) => Monad (BacktrT m) where

    return = pure

    (BTT x) >>= f = BTT $ \g -> x (\a -> (unBTT $ f a) g)


instance (Monad m) => MonadFail (BacktrT m) where

    fail _ = empty

instance MonadTrans BacktrT where
    lift m = BTT (m >>=)


runBTT :: (Monad m) => BacktrT m a -> m [a]
runBTT (BTT f) = f (\x -> return [x])


type LP s = BacktrT (ST s)
type Ref = STRef


runLP :: (forall s. LP s a) -> [a]
runLP m = runST (runBTT m)

newRef :: a -> LP s (Ref s a)
newRef = lift . newSTRef

readRef :: Ref s a -> LP s a
readRef = lift . readSTRef

writeRef :: Ref s a -> a -> LP s ()
writeRef ref a' = BTT $ \cont -> do
    a <- readSTRef ref
    writeSTRef ref a'
    ret <- cont ()
    writeSTRef ref a
    return ret

instance MiniKanren (LP s) (Var s) where

    freshVar = Var <$> newRef Nothing

    unifyVar (Var ref) a = do
        b' <- readRef ref
        case b' of
            Nothing -> writeRef ref (Just a)
            Just b -> unify a b

instance MiniKanrenEval (LP s) (Var s) where

    readVar = readRef . unVar

newtype Var s a = Var { unVar :: Ref s (Maybe a)}
