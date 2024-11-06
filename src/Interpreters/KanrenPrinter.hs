{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, Rank2Types, GADTs #-}
module Interpreters.KanrenPrinter where

import MiniKanren
import Control.Monad.State
import Control.Applicative
import Control.Monad

data ValueTag = Ret | BindRet

data KPrint s a where
    Value :: ValueTag -> a -> ShowS -> KPrint s a
    Unify :: ShowS -> KPrint s ()
    Empty :: ShowS -> KPrint s a

showsKanren :: KPrint s a -> ShowS
showsKanren (Value _ _ p) = p
showsKanren (Unify p) = p
showsKanren (Empty p) = p

bindTag :: ValueTag -> ValueTag
bindTag Ret = BindRet
bindTag t = t

(#>) :: ShowS -> KPrint s a -> KPrint s a
p' #> (Unify p) = Unify (p' . p)
p' #> (Value t v p) = Value (bindTag t) v (p' . p)
p' #> (Empty p) = Empty (p' . p)

(<#) :: KPrint s a -> ShowS -> KPrint s a
(Unify p) <# p' = Unify (p . p')
(Value t v p) <# p' = Value (bindTag t) v (p . p')
(Empty p) <# p' = Empty (p . p')

(##>) :: ShowS -> KPrint s a -> KPrint s a
p' ##> (Value t v p) = Value t v (p' . p)
p' ##> p = p' #> p

instance Functor (KPrint s) where

    fmap = liftA

instance Applicative (KPrint s) where

    pure x = Value Ret x id

    (<*>) = ap

instance Monad (KPrint s) where

    return = pure

    (Value Ret v p) >>= g = p ##> g v
    (Value BindRet v p) >>= g = case g v of
        gv@(Value Ret _ _) -> p #> gv
        gv -> (p . showString " >> ") #> gv
    Empty p >>= _ = Empty p
    (Unify p) >>= g = p #> g (error "Unify value evaluated")

instance Alternative (KPrint s) where

    empty = Empty $ showString "Empty"

    f <|> (Empty _) = f
    (Empty _) <|> g = g
    f <|> g = (showString "(" . showsKanren f . showString ") <|> (") #> g <# showString ")"


instance MonadPlus (KPrint s)

type KPrinter s = StateT Int (KPrint s)

newtype SVar s a = SVar Int deriving (Eq)

instance EqVar (SVar s) where 
    varEq (SVar a) (SVar b) = a == b

instance Show (SVar s a) where
    showsPrec _ (SVar v) = showString "x" . shows v

makeFreshVar :: KPrinter s (Var a (SVar s))
makeFreshVar = SVar <$> do
    v <- get
    modify succ
    return v

instance MiniKanren (KPrinter s) (SVar s) where

    freshVar = do
        v <- makeFreshVar
        lift $ Value Ret v $ showString "fresh $ \\" . shows v . showString " -> "

    argVar arg = do
        v <- makeFreshVar
        lift $ Value Ret v $ showString "argument " . showLogic arg . showString " $ \\" . shows v . showString " -> "

    unifyVar v u = lift $ Unify $ showString "(" . shows v . showString " <=> " . showLogic u . showString ")"


    call (Relation n _) = do
        lift $ Value BindRet () $ showString "call (" . showString n . showString ")"

    call' (Relation _ r) = r

instance MiniKanrenEval (KPrinter s) (SVar s) where
    readVar x = lift $ Value BindRet (Just $ error "evaluated variable value") $ showString "deref " . shows x


printKanren :: (forall s. KPrinter s a) -> String
printKanren p = showsKanren (evalStateT p 0) ""