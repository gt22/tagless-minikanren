module Relations.Appendo where

import MiniKanren
import Types.List
import Control.Applicative

appendo :: (MiniKanren rel var, Unif t) => Logic (List t) var -> Logic (List t) var -> Logic (List t) var -> Relation rel
appendo = relation3 "appendo" $ \x y z -> asum 
    [ do
        x <=> nil
        y <=> z
    , fresh3 $ \h t t' -> do
        x <=> cons h t
        z <=> cons h t'
        call $ appendo t y t'
    ]