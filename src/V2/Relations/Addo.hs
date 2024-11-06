{-# LANGUAGE ScopedTypeVariables, TypeApplications, MultiParamTypeClasses #-}
module V2.Relations.Addo(addoRel) where
import V2.MiniKanren
import MiniKanren (Logic(Free, Ground), Relation(Relation))
import Types.Nat

instance LogicVar Nat where

    _ `vmapMVal` Z = return Z
    f `vmapMVal` (S n) = S <$> (f `vmapM` n)

    showTerm Z = showString "zro"
    showTerm (S n) = showString "suc " . showLogic n

instance Unif Nat where

    unifyVal Z Z = sucunif
    unifyVal (S a) (S b) = unify a b
    unifyVal _ _ = nonunif

instance Deref Nat Int where

    derefVal Z = return 0
    derefVal (S n) = succ <$> deref n


addoRel :: (MiniKanren rel disj conj core var) => Logic Nat var -> Logic Nat var -> Logic Nat var -> Relation rel
addoRel = relation3 "addo" $ \x y z -> fresh2 $ \x' z' -> raiseToRel $ biggestdisj 
    [   [   x <=> zro
        ,   y <=> z
        ]
    ,   [   z <=> suc z'
        ,   call $ addoRel x' y z'
        ,   x <=> suc x'
        ]
    ]
