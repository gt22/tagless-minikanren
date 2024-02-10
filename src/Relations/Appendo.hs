module Relations.Appendo where

import MiniKanren
import Types.Nat
import Types.List
import Control.Applicative

-- liftTuple :: (Monad m) => (m a, m b) -> m (a, b)
-- liftTuple (a,b) = do
--     x <- a
--     y <- b
--     return (x,y)


appendo :: (MiniKanren rel var) => List Nat var -> List Nat var -> List Nat var -> rel ()
appendo x y z = asum 
    [ do
        x === Nil
        y === z
    , fresh3 $ \h t t' -> do
        x === Cons h t 
        z === Cons h t' 
        appendo t y t'
    ]