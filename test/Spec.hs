{-# LANGUAGE FlexibleInstances #-}
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Curve

instance Arbitrary (Term Expression) where
  arbitrary = elements [ Term Type ]

main :: IO ()
main = hspec $ do
  describe "unify" $ do
    prop "_ unifies with anything" $
      \ term -> unified (term `unify` Term Implicit) `shouldBe` Just term
