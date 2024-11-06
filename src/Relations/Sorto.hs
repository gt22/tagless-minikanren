module Relations.Sorto where

import MiniKanren
import Types.Nat
import Types.Bool
import Types.List 
import Control.Applicative

leo :: (MiniKanren rel var) => Logic Nat var -> Logic Nat var -> Logic Boolo var -> Relation rel
leo = relation3 "leo" $ \x y b -> asum 
    [ do 
        x <=> zro
        b <=> trueo
    , fresh $ \zz -> do 
        x <=> suc zz 
        y <=> zro
        b <=> falso 
    , fresh2 $ \x' y' -> do 
        x <=> suc x' 
        y <=> suc y' 
        call $ leo x' y' b 
    ]

noto :: (MiniKanren rel var) => Logic Boolo var -> Logic Boolo var -> Relation rel
noto = relation2 "noto" $ \x notX -> asum 
    [ do 
        x <=> trueo 
        notX <=> falso 
    , do 
        x <=> falso 
        notX <=> trueo 
    ]

gto :: (MiniKanren rel var) => Logic Nat var -> Logic Nat var -> Logic Boolo var -> Relation rel
gto = relation3 "gto" $ \x y b -> fresh $ \b' -> do 
    call $ noto b b' 
    call $ leo x y b'

minmaxo :: (MiniKanren rel var) => Logic Nat var -> Logic Nat var -> Logic Nat var -> Logic Nat var -> Relation rel
minmaxo = relation4 "minmaxo" $ \a b mn mx -> asum 
    [ do 
        mn <=> a 
        mx <=> b 
        call $ leo a b trueo
    , do 
        mx <=> a 
        mn <=> b 
        call $ gto a b trueo
    ]

smallesto :: (MiniKanren rel var) => Logic (List Nat) var -> Logic Nat var -> Logic (List Nat) var -> Relation rel
smallesto = relation3 "smallesto" $ \l s l' -> asum 
    [ do 
        l <=> cons s nil
        l' <=> nil 
    , fresh5 $ \h t s' t' mx -> do 
        l' <=> cons mx t' 
        l <=> cons h t 
        call $ minmaxo h s' s mx 
        call $ smallesto t s' t'
    ]

sorto :: (MiniKanren rel var) => Logic (List Nat) var -> Logic (List Nat) var -> Relation rel
sorto = relation2 "sorto" $ \x y -> asum 
    [ do 
        x <=> nil 
        y <=> nil 
    , fresh3 $ \s xs xs' -> do 
        y <=> cons s xs' 
        call $ sorto xs xs' 
        call $ smallesto x s xs 
    ]
