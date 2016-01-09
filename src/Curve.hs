module Curve where

data Term
  = Type
  | Implicit
  | Lambda Term Term
  | Application Term Term
