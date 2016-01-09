{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}
module Curve where

import Data.Functor.Classes

data Name
  = Local Int
  | Global String
  deriving (Show, Eq)

data Expression term
  = Type
  | Implicit
  | Variable Name
  | Lambda Int term term
  | Application term term
  deriving (Show, Eq, Functor, Foldable)

instance Eq1 Expression where
  eq1 = (==)


data Term f = Term { out :: f (Term f) }
type Term' = Term Expression

instance Eq1 f => Eq (Term f) where
  a == b = out a `eq1` out b

data Unification f = Unification (f (Unification f)) | Conflict (Term f) (Term f)
type Unification' = Unification Expression

into :: Functor f => Term f -> Unification f
into term = Unification $ into <$> out term

unified :: Unification' -> Maybe Term'
unified (Unification _) = Nothing
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

  (Type, Type) -> into expected

  (Variable n1, Variable n2) | n1 == n2 -> Unification $ Variable n2
  (Application a1 b1, Application a2 b2) -> Unification $ Application (unify a1 a2) (unify b1 b2)
  (Lambda i1 a1 b1, Lambda i2 a2 b2) | i1 == i2 -> Unification $ Lambda i2 (unify a1 a2) (unify b1 b2)

  _ -> Conflict expected actual
