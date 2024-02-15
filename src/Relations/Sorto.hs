module Relations.Sorto where

import MiniKanren
import Types.Nat
import Types.Bool
import Types.List 
import Control.Applicative

leo :: (MiniKanren rel var) => Nat var -> Nat var -> Boolo var -> rel ()
leo x y b = asum 
    [ do 
        x === Z 
        b === Trueo 
    , fresh $ \zz -> do 
        x === S zz 
        y === Z 
        b === Falso 
    , fresh2 $ \x' y' -> do 
        x === S x' 
        y === S y' 
        leo x' y' b 
    ]

noto :: (MiniKanren rel var) => Boolo var -> Boolo var -> rel () 
noto x notX = asum 
    [ do 
        x === Trueo 
        notX === Falso 
    , do 
        x === Falso 
        notX === Trueo 
    ]

gto :: (MiniKanren rel var) => Nat var -> Nat var -> Boolo var -> rel ()
gto x y b = fresh $ \b' -> do 
    noto b b' 
    leo x y b'

minmaxo :: (MiniKanren rel var) => Nat var -> Nat var -> Nat var -> Nat var -> rel () 
minmaxo a b mn mx = asum 
    [ do 
        mn === a 
        mx === b 
        leo a b Trueo 
    , do 
        mx === a 
        mn === b 
        gto a b Trueo       
    ]

smallesto :: (MiniKanren rel var) => List Nat var -> Nat var -> List Nat var -> rel () 
smallesto l s l' = asum 
    [ do 
        l === Cons s Nil 
        l' === Nil 
    , fresh5 $ \h t s' t' mx -> do 
        l' === Cons mx t' 
        l === Cons h t 
        minmaxo h s' s mx 
        smallesto t s' t'
    ]

sorto :: (MiniKanren rel var) => List Nat var -> List Nat var -> rel () 
sorto x y = asum 
    [ do 
        x === Nil 
        y === Nil 
    , fresh3 $ \s xs xs' -> do 
        y === Cons s xs' 
        sorto xs xs' 
        smallesto x s xs 
    ]
