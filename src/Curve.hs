{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}
module Curve where

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
