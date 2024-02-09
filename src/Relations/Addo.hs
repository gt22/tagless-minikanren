module Relations.Addo where

import MiniKanren
import Types.Nat
import Control.Applicative

liftTuple :: (Monad m) => (m a, m b) -> m (a, b)
liftTuple (a,b) = do
    x <- a
    y <- b
    return (x,y)


addo :: (MiniKanren rel var) => Nat var -> Nat var -> Nat var -> rel ()
addo x y z = asum 
    [ do
        x === Z
        y === z
    , fresh2 $ \x' z' -> do
        x === S x'
        z === S z'
        addo x' y z'
    ]