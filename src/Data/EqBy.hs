module Data.EqBy where

class EqBy f where
  eq :: (a -> a -> Bool) -> f a -> f a -> Bool
