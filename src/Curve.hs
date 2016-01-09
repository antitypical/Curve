{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}
module Curve where

data Name
  = Local Int
  | Global String

data Expression term
  = Type
  | Implicit
  | Lambda term term
  | Application term term
  deriving (Show, Eq, Functor, Foldable)

data Term f = Term { out :: f (Term f) }
type Term' = Term Expression

data Unification f = Unification (f (Unification f)) | Conflict (Term f) (Term f)
type Unification' = Unification Expression

into :: Functor f => Term f -> Unification f
into term = Unification $ into <$> out term

unify :: Term' -> Term' -> Unification'
unify expected actual = case (out expected, out actual) of
  (_, Implicit) -> into expected

  (Type, Type) -> into expected
  (Application a1 b1, Application a2 b2) -> Unification $ Application (unify a1 a2) (unify b1 b2)
  (Lambda a1 b1, Lambda a2 b2) -> Unification $ Lambda (unify a1 a2) (unify b1 b2)

  _ -> Conflict expected actual
