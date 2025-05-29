{-# LANGUAGE Rank2Types, LambdaCase, ApplicativeDo #-}
module TFKanren.Utils.Type
( module TFKanren.Core.Internal.Logic
, defaultUnif
, con0, con1, con2, con3
, quote0, quote1, quote2, quote3
, LogicType(..)
, empty, (<|>), asum
, showLogic, project', reify'
) where
import TFKanren.Core.Internal.Logic
import TFKanren.Utils.Logic
import Text.Printf (printf)
import Unsafe.Coerce (unsafeCoerce)
import Control.Applicative (Alternative (empty, (<|>)), asum)
import Control.Monad (guard)
import Data.Foldable (traverse_)

type L' = Logic

defaultUnif :: (Alternative rel, LogicType a) => (forall t. LogicType t => L' t var -> L' t var -> rel ()) -> WithLogic a var -> WithLogic a var -> rel ()
defaultUnif unif x y = do
    let (con_x, xs) = quote x
    let (con_y, ys) = quote y
    guard (name con_x == name con_y)
    traverse_ (\((Field xi), (Field yi)) -> unif xi (unsafeCoerce yi)) (zip xs ys)
    pure ()

reifyError :: String -> Int -> [x] -> a
reifyError con expected actual = error $ printf "Constructor %s expected %d fields, got %d" con expected (length actual)

con0 :: String -> (forall var. WithLogic t var) -> Constructor t
con0 c r = Constructor c $ \case { [] -> r; xs -> reifyError c 0 xs}

con1 :: String -> (forall var. L' a var -> WithLogic t var) -> Constructor t
con1 c r = Constructor c $ \case
    [Field x] -> r (unsafeCoerce x)
    xs -> reifyError c 1 xs

con2 :: String -> (forall var. L' a var -> L' b var -> WithLogic t var) -> Constructor t
con2 c r = Constructor c $ \case
    [Field x, Field y] -> r (unsafeCoerce x) (unsafeCoerce y)
    xs -> reifyError c 2 xs

con3 :: String -> (forall var. L' a var -> L' b var -> L' c var -> WithLogic t var) -> Constructor t
con3 c r = Constructor c $ \case
    [Field x, Field y, Field z] -> r (unsafeCoerce x) (unsafeCoerce y) (unsafeCoerce z)
    xs -> reifyError c 3 xs

quote0 :: String -> (forall var. WithLogic t var) -> (Constructor t, [Field t v])
quote0 c r = (con0 c r, [])

quote1 :: (LogicType a) => String -> (forall var. L' a var -> WithLogic t var) -> L' a v -> (Constructor t, [Field t v])
quote1 c r x = (con1 c r, [Field x])

quote2 :: (LogicType a, LogicType b) => String -> (forall var. L' a var -> L' b var -> WithLogic t var) -> L' a v -> L' b v -> (Constructor t, [Field t v])
quote2 c r x y = (con2 c r, [Field x, Field y])

quote3 :: (LogicType a, LogicType b, LogicType c) => String -> (forall var. L' a var -> L' b var -> L' c var -> WithLogic t var) -> L' a v -> L' b v -> L' c v -> (Constructor t, [Field t v])
quote3 c r x y z = (con3 c r, [Field x, Field y, Field z])