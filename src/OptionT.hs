module OptionT where
import Control.Monad
import Data.Maybe (fromMaybe)
import Control.Applicative

newtype OptionT m a = OptionT { runOptionT :: Maybe (m a) }

instance (Functor m) => Functor (OptionT m) where

    fmap f (OptionT x) = OptionT $ fmap (fmap f) x

instance (Applicative m) => Applicative (OptionT m) where

    pure x = OptionT $ pure (pure x)

    (OptionT f) <*> (OptionT x) = OptionT $ liftA2 (<*>) f x

instance (MonadPlus m) => Monad (OptionT m) where

    return = pure

    (OptionT Nothing) >>= _ = OptionT Nothing
    (OptionT (Just x)) >>= f = OptionT $ Just $ x >>= fromMaybe empty . runOptionT . f

instance (Alternative m) => Alternative (OptionT m) where

    empty = OptionT Nothing

    a <|> (OptionT Nothing) = a
    (OptionT Nothing) <|> b = b
    (OptionT a) <|> (OptionT b) = OptionT $ liftA2 (<|>) a b 

instance (MonadPlus m) => MonadPlus (OptionT m)

liftOption :: m a -> OptionT m a
liftOption = OptionT . Just

mapOptionT :: (m a -> n b) -> OptionT m a -> OptionT n b
mapOptionT f (OptionT x) = OptionT $ f <$> x

fromOptionT :: m a -> OptionT m a -> m a
fromOptionT a (OptionT b) = fromMaybe a b