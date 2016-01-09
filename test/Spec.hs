{-# LANGUAGE FlexibleInstances #-}
import Test.QuickCheck
import Curve

instance Arbitrary (Term Expression) where
  arbitrary = elements [ Term Type ]

main :: IO ()
main = putStrLn "Test suite not yet implemented"
