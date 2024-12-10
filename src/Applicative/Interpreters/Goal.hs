{-# LANGUAGE GADTs, FlexibleInstances, MultiParamTypeClasses, ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
module Applicative.Interpreters.Goal where
import Applicative.MiniKanren
import MiniKanren (Logic(Free, Ground), Var, LogicVar, Relation(Relation), vmap)
import Control.Applicative
import Control.Monad.State
import Unsafe.Coerce (unsafeCoerce)

newtype GoalVar s a = GVar Int deriving (Show, Eq)

instance EqVar (GoalVar s) where

    (GVar x) `varEq` (GVar y) = x == y

type Term s x = Logic x (GoalVar s)
type TVar s x = Var x (GoalVar s)

data Goal s a where

    Fail :: Goal s a
    Trivial :: Goal s ()
    Raise :: Goal s () -> a -> Goal s a
    Unify :: (Unif a) => TVar s a -> Term s a -> Goal s ()
    Conj :: [Goal s ()] -> Goal s ()
    Disj :: [Goal s a] -> Goal s a
    Fresh :: (TVar s x -> Goal s a) -> Goal s a
    Arg :: (LogicVar x) => Term s x -> (TVar s x -> Goal s a) -> Goal s a

    Invoke :: String -> Goal s () -> Goal s ()
    Invoke' :: String -> Goal s () -> Goal s ()


instance Functor (Goal s) where

    _ `fmap` Fail = Fail
    f `fmap` Trivial = Raise Trivial (f ())
    f `fmap` (Raise g x) = Raise g (f x)
    f `fmap` g@(Unify _ _) = Raise g (f ())
    f `fmap` g@(Conj _) = Raise g (f ())
    f `fmap` (Disj conjs) = Disj $ fmap f <$> conjs
    f `fmap` (Fresh g) = Fresh $ fmap f . g
    f `fmap` (Arg x g) = Arg x $ fmap f . g
    f `fmap` g@(Invoke _ _) = Raise g (f ())
    f `fmap` g@(Invoke' _ _) = Raise g (f ())


conj :: Goal s () -> Goal s () -> Goal s ()
conj (Conj xs) (Conj ys) = Conj $ xs ++ ys
conj x (Conj xs) = Conj $ x:xs
conj (Conj xs) x = Conj $ xs ++ [x]
conj x y = Conj [x, y]

attach :: Goal s () -> Goal s a -> Goal s a
attach _ Fail = Fail
attach g Trivial = g
attach g (Raise g' a) = Raise (conj g g') a
attach g g'@(Unify _ _) = conj g g'
attach g g'@(Conj _) = conj g g'
attach g (Fresh g') = Fresh $ \v -> attach g (g' v)
attach g (Arg x g') = Arg x $ \v -> attach g (g' v)
attach g (Disj gs) = Disj $ attach g <$> gs
attach g g'@(Invoke _ _) = conj g g'
attach g g'@(Invoke' _ _) = conj g g'


instance Applicative (Goal s) where

    pure = Raise Trivial

    Fail <*> _ = Fail
    (Raise x f) <*> y = attach x (f <$> y)
    (Disj xs) <*> y = Disj $ (<*> y) <$> xs
    (Fresh g) <*> y = Fresh $ \v -> g v <*> y
    (Arg x g) <*> y = Arg x $ \v -> g v <*> y

instance Alternative (Goal s) where

    empty = Fail

    (Disj as) <|> (Disj bs) = Disj $ as ++ bs
    a <|> (Disj bs) = Disj $ a:bs
    (Disj as) <|> b = Disj $ as ++ [b]
    a <|> b = Disj [a, b]

instance MiniKanren (Goal s) (GoalVar s) where

    fresh_ = Fresh
    argument_ = Arg

    unifyVar = Unify

    call (Relation n r) = Invoke n r
    call' (Relation n r) = Invoke' n r


data VarMapping s var = VarMapping {
    varSubst :: forall a. TVar s a -> Var a var,
    varGen :: Int
    }
type KanrenMap s var a = State (VarMapping s var) a

setVar :: TVar s a -> Var a var -> VarMapping s var -> VarMapping s var
setVar v u s = s { varSubst = \v' -> if v `varEq` v' then unsafeCoerce u else varSubst s v' }

makeVar :: KanrenMap s var (GoalVar s a)
makeVar = do
    n <- gets varGen
    modify $ \s -> s { varGen = succ $ varGen s }
    return $ GVar n

subst :: TVar s a -> KanrenMap s var (Var a var)
subst v = do
    s <- get
    return $ varSubst s v

svmap :: (LogicVar a) => Term s a -> KanrenMap s var (Logic a var)
svmap v = do
    s <- get
    return $ varSubst s `vmap` v

scoped :: (Monad m) => (s -> s) -> StateT s m a -> StateT s m a
scoped f act = gets f >>= lift . evalStateT act

evalGoal :: (MiniKanren rel var) => Goal s a -> KanrenMap s var (rel a)
evalGoal Fail = return empty
evalGoal Trivial = return $ pure ()
evalGoal (Raise g x) = do
    g' <- evalGoal g
    return $ x <$ g'
evalGoal (Unify a b) = do
    a' <- subst a
    b' <- svmap b
    return $ unifyVar a' b'
evalGoal (Conj []) = return $ pure ()
evalGoal (Conj [a]) = evalGoal a
evalGoal (Conj (x:xs)) = do
    x' <- evalGoal x
    xs' <- evalGoal (Conj xs)
    return $ x' *> xs'
evalGoal (Disj []) = return empty
evalGoal (Disj [a]) = evalGoal a
evalGoal (Disj (x:xs)) = do
    x' <- evalGoal x
    xs' <- evalGoal (Disj xs)
    return $ x' <|> xs'
evalGoal (Fresh f) = do
    v <- makeVar
    s <- get
    return $ fresh_ $ \v' -> evalState (modify (setVar v v') >> evalGoal (f v)) s
evalGoal (Arg arg f) = do
    arg' <- svmap arg
    v <- makeVar
    s <- get
    return $ argument_ arg' $ \v' -> evalState (modify (setVar v v') >> evalGoal (f v)) s
evalGoal (Invoke n r) = do
    s <- get
    let r' = evalState (evalGoal r) s
    return $ call (relation n r')
evalGoal (Invoke' n r) = do
    s <- get
    let r' = evalState (evalGoal r) s
    return $ call' (relation n r')

bindVar :: Var a var -> KanrenMap s var (Term s a)
bindVar x' = do
    x <- makeVar
    modify (setVar x x')
    return $ Free x

evalGoal2 :: (MiniKanren rel var) => (Term s a -> Term s b -> Goal s t) -> Var a var -> Var b var -> KanrenMap s var (rel t)
evalGoal2 f a' b' = do
    a <- bindVar a'
    b <- bindVar b'
    evalGoal $ f a b

emptyMapping :: VarMapping s var
emptyMapping = VarMapping { varSubst = \v -> error $ "Unknown variable: " ++ show v, varGen = 0 }

runGoal :: (MiniKanren rel var) => Goal s t -> rel t
runGoal g = evalState (evalGoal g) emptyMapping

runGoal2 :: (MiniKanren rel var) => (forall s. Term s a -> Term s b -> Goal s t) -> Var a var -> Var b var -> rel t
runGoal2 f a' b' = evalState (evalGoal2 f a' b') emptyMapping

runGoal2' :: (MiniKanrenEval rel var, Deref a a', Deref b b') => (forall s. Term s a -> Term s b -> Goal s t) -> rel (a', b')
runGoal2' f = fresh2' $ \x y -> do
    _ <- runGoal2 f x y
    a <- deref (Free x)
    b <- deref (Free y)
    return (a, b)

reverseGoal :: Goal s a -> Goal s a
reverseGoal (Conj xs) = Conj (reverse xs)
reverseGoal (Disj xs) = Disj $ reverseGoal <$> xs
reverseGoal (Raise g x) = Raise (reverseGoal g) x
reverseGoal (Fresh f) = Fresh $ \v -> reverseGoal $ f v
reverseGoal (Arg x f) = Arg x $ \v -> reverseGoal $ f v
reverseGoal g = g