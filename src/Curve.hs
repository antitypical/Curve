module Curve where

data Expression term
  = Type
  | Implicit
  | Lambda term term
  | Application term term

data Term f = Term (f (Term f))

data Unification f = Unification (f (Unification f)) | Conflict (Term f) (Term f)
