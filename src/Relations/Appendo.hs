module Relations.Appendo where

import MiniKanren
import Types.List
import Control.Applicative

appendo :: (MiniKanren rel var, Unif t) => Logic (List t) var -> Logic (List t) var -> Logic (List t) var -> rel ()
appendo x y z = asum 
    [ do
        x === Nil
        y <=> z
    , fresh3 $ \h t t' -> do
        x === Cons h t
        z === Cons h t'
        appendo t y t'
    ]