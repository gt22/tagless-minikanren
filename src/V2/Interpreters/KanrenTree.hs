{-# LANGUAGE GADTs, DeriveFunctor, MultiParamTypeClasses, FlexibleInstances #-}
module V2.Interpreters.KanrenTree where
import V2.MiniKanren
import MiniKanren(Var, Logic, Relation(Relation))
import Control.Monad (ap)
import Control.Applicative (liftA)
import Control.Monad.State

newtype TreeVar s a = KVar Int deriving (Show, Eq, Functor)

data Core s a where
    Fail :: Core s ()
    Unit :: Core s ()
    Unify :: (Unif a) => Var a (TreeVar s) -> (Logic a (TreeVar s)) -> Core s ()
    Call :: String -> KanrenAct s () -> Core s ()
    TransparentCall :: String -> KanrenAct s () -> Core s ()

newtype Conj s a = Conj [Core s a]
newtype Disj s a = Disj [Conj s a]
data KanrenTree s a where
    Const :: a -> KanrenTree s a
    Content :: Disj s () -> KanrenTree s ()
    Attach :: KanrenTree s a -> (a -> KanrenTree s b) -> KanrenTree s b
    Fresh :: Var x (TreeVar s) -> KanrenTree s a -> KanrenTree s a
    Arg :: (LogicVar x) => Logic x (TreeVar s) -> Var x (TreeVar s) -> KanrenTree s a -> KanrenTree s a


instance Functor (KanrenTree s) where

    fmap = liftA


instance Applicative (KanrenTree s) where

    pure = Const

    (<*>) = ap

instance Monad (KanrenTree s) where

    return = pure

    (Const x) >>= f = f x
    (Fresh v t) >>= f = Fresh v (t >>= f)
    (Arg x v t) >>= f = Arg x v (t >>= f)
    t >>= f = Attach t f


type KanrenAct s = StateT Int (KanrenTree s)

makeFreshVar :: KanrenAct s (Var a (TreeVar s))
makeFreshVar = KVar <$> do
    v <- get
    modify succ
    return v

instance MiniKanren (KanrenAct s) (Disj s) (Conj s) (Core s) (TreeVar s) where

    raiseToConj x = Conj [x]
    raiseToDisj x = Disj [x]
    raiseToRel = lift . Content

    unifyVar = Unify
    call (Relation n r) = Call n r
    call' (Relation n r) = TransparentCall n r

    freshVar f = do
        v <- makeFreshVar
        mapStateT (Fresh v) (f v)

    argVar arg f = do
        v <- makeFreshVar
        mapStateT (Arg arg v) (f v)

    nonunif = Fail
    sucunif = Unit

    trivial = Conj []
    failing = Disj []

    conj a (Conj b) = Conj (a : b)
    disj a (Disj b) = Disj (a : b)

    bigconj = Conj
    bigdisj = Disj