module Loeb where

fs :: [[Int] -> Int]
fs = [ const 1
     , succ . (!! 0)
     , succ . (!! 1)
     , succ . (!! 2)
     ]

loeb :: Functor f => f (f a -> a) -> f a
loeb x = go where
  go = fmap ($go) x
