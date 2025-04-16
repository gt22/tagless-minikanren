{-# LANGUAGE TypeFamilies, DeriveFunctor, FlexibleInstances #-}
module TFKanren.Interpreters.Typeless.Typeless where


import TFKanren.Core.Internal.Kanren
import TFKanren.Core.Internal.Logic
import qualified TFKanren.Interpreters.Typeless.Def as Def
import qualified TFKanren.Interpreters.Typeless.Syntax as S
import qualified Data.Map as Map
import Control.Monad.State


type BaseVar = S.S
type Term = S.Term BaseVar
type Def = Def.Def S.G BaseVar

toTermP' :: (LogicVar a) => a (KVar Typeless) -> S.Term BaseVar
toTermP' x = let (con, elems) = quote x in S.C (name con) $ map (\(Field _ x') -> toTermP x') elems

toTermP :: (LogicVar a) => Logic a (KVar Typeless) -> S.Term BaseVar
toTermP (Free v) = S.V (unsyn v)
toTermP (Ground x) = toTermP' x


data BaseKState = BaseKState { nextVar :: Int, defs :: Map.Map String Def, args :: [(BaseVar, Term)] }

nullState :: BaseKState
nullState = BaseKState { nextVar = 0, defs = Map.empty, args = [] }

data RaisedGoal t = RG { goal :: S.G BaseVar, value :: [t] } deriving (Show, Eq, Functor)

g :: S.G BaseVar -> RaisedGoal ()
g x = RG { goal = x, value = [()] }

clearingConj :: Eq a => S.G a -> S.G a -> S.G a
clearingConj g1 g2 | g1 == S.success = g2
                   | g2 == S.success = g1
                   | otherwise = S.flatConj g1 g2

instance Applicative RaisedGoal where

    pure x = RG { goal = S.success, value = [x] }

    f <*> x = RG { goal = clearingConj (goal f) (goal x), value = value f <*> value x }

clearingDisj :: Eq a => S.G a -> S.G a -> S.G a
clearingDisj g1 g2 | g1 == S.failure = g2
                   | g2 == S.failure = g1
                   | otherwise = S.flatDisj g1 g2

instance Alternative RaisedGoal where

    empty = RG { goal = S.failure, value = [] }

    x <|> y = RG { goal = clearingDisj (goal x) (goal y), value = value x <|> value y }

type Computation = State BaseKState

updateDefs :: Def -> Map.Map String Def -> Map.Map String Def
updateDefs d m = Map.insert (Def.getName d) d m

hasDef :: String -> Map.Map String Def -> Bool
hasDef = Map.member

data Typeless t = PK { runBodylessPK :: Computation (RaisedGoal t), runPK :: Computation (RaisedGoal t) } deriving (Functor)

instance Applicative Typeless where

    pure x = PK (pure $ pure x) (pure $ pure x)

    (PK f h) <*> (PK x y) = PK (liftA2 (<*>) f x) (liftA2 (<*>) h y)

instance Alternative Typeless where

    empty = PK (pure empty) (pure empty)

    (PK x a) <|> (PK y b) = PK (liftA2 (<|>) x y) (liftA2 (<|>) a b)

integrateDef' :: String -> Typeless t -> Map.Map String Def -> Map.Map String Def
integrateDef' n r m | hasDef n m = m
                    | otherwise = Map.insert n (Def.Def n (fst <$> args s') (goal rg)) (defs s')
    where
        (rg, s') = runState (runPK r) (nullState { defs = Map.insert n (error "Anti-recursive marker-def accessed") m })

instance Kanren Typeless where

    newtype instance (KVar Typeless) t = SynVar { unsyn :: BaseVar } deriving (Eq, Ord, Show, Functor)

    fresh_ x f = PK (doFresh >>= runBodylessPK) (doFresh >>= runPK) 
        where
            updateArgs FreshVar _ s = args s
            updateArgs (ArgVar x') v s = args s ++ [(v, toTermP x')]

            doFresh = do
                v <- gets nextVar
                modify $ \s -> s { nextVar = succ v, args = updateArgs x v s }
                pure $ f (SynVar v)
    unify a b = PK (pure empty) $ pure $ g $ (toTermP a) S.=== (toTermP b)
    call_ _ (Relation n pk) = PK (pure empty) $ do
        modify $ \s -> s { defs = integrateDef' n pk (defs s) }
        let s' = execState (runBodylessPK pk) nullState
        pure $ g $ S.call n (snd <$> args s')
    
    displayVar (SynVar x) = show x


toTypeless :: Relation Typeless -> [Def]
toTypeless r = Map.elems $ defs $ execState (runPK $ call_ Transparent r) nullState

freeArg :: Logic a (KVar Typeless)
freeArg = error "Free arg accessed"