module Data.EqBy where

class EqBy f where
  eqBy :: (a -> a -> Bool) -> f a -> f a -> Bool
