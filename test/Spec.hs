{-# LANGUAGE FlexibleInstances #-}
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Curve

instance Arbitrary Name where
  arbitrary = oneof [ Local <$> arbitrary, Global <$> arbitrary ]

instance Arbitrary (Term Expression) where
  arbitrary = elements [ Term Type ]

main :: IO ()
main = hspec $ do
  describe "unify" $ do
    prop "_ with x" $
      \ term -> term `unify` Term Implicit `shouldBe` into term

    prop "x with _" $
      \ term -> Term Implicit `unify` term `shouldBe` into term
