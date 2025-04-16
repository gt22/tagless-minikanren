{-# LANGUAGE ApplicativeDo #-}
module TFKanren.Examples.Relations.Balance(lto, leo, gto, geo, maxo, deptho, similaro, balancedo, balanceo) where


import TFKanren.Examples.Types.Nat
import TFKanren.Examples.Types.Tree
import TFKanren.Core.Kanren
import TFKanren.Examples.Types.List

lto :: (Kanren rel) => L Nat rel -> L Nat rel -> Relation rel
lto = relation2 "lto" $ \l g -> conde 
    [ l <=> suc g
    , fresh2 $ \l' g' -> do
        l <=> suc l'
        g <=> suc g'
        call $ leo l' g'
        pure ()
    ]

leo :: (Kanren rel) => L Nat rel -> L Nat rel -> Relation rel
leo = relation2 "leo" $ \l g -> conde [l <=> g, call $ l `lto` g]

gto :: (Kanren rel) => L Nat rel -> L Nat rel -> Relation rel
gto = relation2 "gto" $ \g l -> embed $ l `lto` g

geo :: (Kanren rel) => L Nat rel -> L Nat rel -> Relation rel
geo = relation2 "geo" $ \g l -> embed $ g `leo` l

maxo :: (Kanren rel) => L Nat rel -> L Nat rel -> L Nat rel -> Relation rel
maxo = relation3 "maxo" $ \x y mx -> conde 
    [ do
        call $ x `leo` y
        mx <=> y
        pure ()
    , do
        call $ x `gto` y
        mx <=> x
        pure ()
    ]

deptho :: (Kanren rel, LogicVar elem) => L (Tree elem) rel -> L Nat rel -> Relation rel
deptho = relation2 "deptho" $ \t d -> conde 
    [ do
        t <=> leaf
        d <=> zro
        pure ()
    , fresh3 $ \l x r -> fresh3 $ \ld rd d' -> do
        t <=> node l x r
        d <=> suc d'
        call $ deptho l ld
        call $ deptho r rd
        call $ maxo ld rd d'
        pure ()
    ]

similaro :: (Kanren rel) => L Nat rel -> L Nat rel -> Relation rel
similaro = relation2 "similaro" $ \x y -> conde [ x <=> y, x <=> suc y, y <=> suc x ]

balancedo :: (Kanren rel, LogicVar elem) => L (Tree elem) rel -> Relation rel
balancedo = relation "balancedo" $ \t -> conde 
    [ t <=> leaf
    , fresh5 $ \l x r dl dr -> do
        t <=> node l x r
        call $ deptho l dl
        call $ deptho r dr
        call $ similaro dl dr
        call $ balancedo l
        call $ balancedo r
        pure ()
    ]

appendo :: (Kanren rel, LogicVar elem) => L (List elem) rel -> L (List elem) rel -> L (List elem) rel -> Relation rel
appendo = relation3 "appendo" $ \x y xy -> conde
    [ do
        x <=> nil
        y <=> xy
        pure ()
    , fresh3 $ \h x' xy' -> do
        x <=> cons h x'
        xy <=> cons h xy'
        call $ appendo x' y xy'
        pure ()
    ]

traverso :: (Kanren rel, LogicVar elem) => L (Tree elem) rel -> L (List elem) rel -> Relation rel
traverso = relation2 "traverso" $ \t e -> conde 
    [ do
        t <=> leaf
        e <=> nil
        pure ()
    , fresh5 $ \l x r el er -> do
        t <=> node l x r
        call $ traverso l el
        call $ traverso r er
        call $ appendo el (cons x er) e
        pure ()
    ]

balanceo :: (Kanren rel, LogicVar elem) => L (Tree elem) rel -> L (Tree elem) rel -> Relation rel
balanceo = relation2 "balanceo" $ \v u -> fresh $ \e -> do
    call $ traverso v e
    call $ traverso u e
    call $ balancedo u
    pure ()