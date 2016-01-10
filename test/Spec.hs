{-# LANGUAGE FlexibleInstances #-}
import qualified Data.Set as Set
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Curve

instance Arbitrary Name where
  arbitrary = oneof [ Local <$> arbitrary, Global <$> arbitrary ]

instance Arbitrary (Term Expression) where
  arbitrary = inScope []
    where inScope names = frequency $
              (4, pure (Term Type))
            : (4, pure (Term Implicit))
            : (1, Term <$> (Application <$> inScope names <*> inScope names))
            : (1, Term <$> (Lambda (length names) <$> inScope names <*> inScope (Local (length names) : names)))
            : ((,) 4 . pure . Term . Variable <$> names)

  shrink term = filter (/= term) $ case out term of
    Application a b -> a : b : shrink a ++ shrink b ++ (Term <$> (Application <$> a : shrink a <*> b : shrink b))
    Lambda i t b -> t : b : shrink t ++ shrink b ++ (Term <$> (Lambda i <$> t : shrink t <*> b : shrink b))
    _ -> []

main :: IO ()
main = hspec $ do
  describe "unify" $ do
    prop "_ with x" $
      \ term -> term `unify` Term Implicit `shouldBe` into term

    prop "x with _" $
      \ term -> Term Implicit `unify` term `shouldBe` into term

    prop "reflexivity" $
      \ term -> unify term term `shouldBe` into term

    prop "symmetry" $
      \ a b -> a `unify` b `shouldBe` flipUnification (b `unify` a)

  describe "freeVariables" $ do
    prop "variables are free in themselves" $
      \ name -> freeVariables (Term (Variable name)) `shouldBe` Set.singleton name

    prop "lambdas are shadowing" $
      \ name t b -> freeVariables (Term (Lambda name t b)) `shouldSatisfy` Set.notMember (Local name)

  describe "showsLevelPrec" $ do
    prop "parenthesizes right-nested applications" $
      \ a b c -> show (Term $ Application a (Term $ Application b c)) `shouldBe` show a ++ " (" ++ showsPrec 10 (Term $ Application b c) ")"

    prop "shows non-dependent function types with an arrow operator" $
      \ a b -> showsLevelPrec True 0 (Term $ Lambda 0 a b) "" `shouldBe` showsLevel True a " → " ++ showsLevel True b ""

    prop "parentheses left-nested non-dependent function types" $
      \ a b c -> showsLevelPrec True 0 (Term $ Lambda 0 (Term $ Lambda 1 a b) c) "" `shouldBe` "(" ++ showsLevelPrec True 0 (Term $ Lambda 1 a b) ") → " ++ showsLevel True c ""

  where flipUnification (Conflict a b) = Conflict b a
        flipUnification (Unification out) = Unification $ flipUnification <$> out
