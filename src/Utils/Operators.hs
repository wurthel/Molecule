module Utils.Operators where

(||>) :: (a -> b) -> (b -> c) -> (a -> c)
(||>) = flip (.)

infixl 9 ||>

(>->) :: a -> (a -> b) -> b
(>->) x f = f x

infixl 1 >->
