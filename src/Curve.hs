{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Curve where

import Data.Functor.Classes

data Name
  = Local Int
  | Global String
  deriving (Show, Eq, Ord)

data Expression term
  = Type
  | Implicit
  | Variable Name
  | Lambda Int term term
  | Application term term
  deriving (Show, Eq, Functor, Foldable, Traversable)

data Term f = Term { out :: f (Term f) }
type Term' = Term Expression

data Unification f = Unification (f (Unification f)) | Conflict (Term f) (Term f)
type Unification' = Unification Expression

into :: Functor f => Term f -> Unification f
into term = Unification $ into <$> out term

unified :: Unification' -> Maybe Term'
unified (Unification expression) = Term <$> traverse unified expression
unified (Conflict _ _) = Nothing


rename :: Int -> Int -> Term' -> Term'
rename old new term | old == new = term
rename old new term = Term $ case out term of
  Variable (Local name) | name == old -> Variable $ Local new
  Lambda name t b -> if name == old
    then Lambda name (rename old new t) b
    else Lambda name (rename old new t) (rename old new b)
  Application a b -> Application (rename old new a) (rename old new b)
  other -> other

substitute :: Int -> Term' -> Term' -> Term'
substitute name withTerm inScope = case out inScope of
  Variable (Local n) | n == name -> withTerm
  Lambda n inType inBody -> if n == name
    then Term $ Lambda n (substitute name withTerm inType) inBody
    else Term $ Lambda n (substitute name withTerm inType) (substitute name withTerm inBody)
  Application inA inB -> Term $ Application (substitute name withTerm inA) (substitute name withTerm inB)
  _ -> inScope

unify :: Term' -> Term' -> Unification'
unify expected actual = case (out expected, out actual) of
  (_, Implicit) -> into expected
  (Implicit, _) -> into actual

  (Type, Type) -> into expected

  (Variable n1, Variable n2) | n1 == n2 -> Unification $ Variable n2
  (Application a1 b1, Application a2 b2) -> Unification $ Application (unify a1 a2) (unify b1 b2)
  (Lambda i1 a1 b1, Lambda i2 a2 b2) | i1 == i2 -> Unification $ Lambda i2 (unify a1 a2) (unify b1 b2)

  _ -> Conflict expected actual


cata :: Functor f => (f a -> a) -> Term f -> a
cata f = f . fmap (cata f) . out

para :: Functor f => (f (Term f, a) -> a) -> Term f -> a
para f = f . fmap fanout . out
  where fanout a = (a, para f a)


instance Eq1 Expression where
  eq1 = (==)

instance Show1 Expression where
  showsPrec1 = showsPrec


instance Eq1 f => Eq (Term f) where
  a == b = out a `eq1` out b

instance Show1 f => Show (Term f) where
  showsPrec i = showsPrec1 i . out

instance Eq1 f => Eq (Unification f) where
  Unification a == Unification b = a `eq1` b
  Conflict a1 b1 == Conflict a2 b2 = a1 == a2 && b1 == b2
  _ == _ = False

instance Show1 f => Show (Unification f) where
  showsPrec i (Unification out) rest = showsPrec1 i out rest
  showsPrec _ (Conflict a b) out = "Expected: " ++ show a ++ "\n"
                                ++ "  Actual: " ++ show b ++ "\n" ++ out
