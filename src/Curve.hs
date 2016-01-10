{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, FlexibleInstances #-}
module Curve where

import Data.Functor.Classes
import qualified Data.Set as Set

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


freeVariables :: Term' -> Set.Set Name
freeVariables = cata inExpression
  where inExpression expression = case expression of
          Variable name -> Set.singleton name
          Lambda i t b -> Set.delete (Local i) b `Set.union` t
          Application a b -> a `Set.union` b
          _ -> mempty


cata :: Functor f => (f a -> a) -> Term f -> a
cata f = f . fmap (cata f) . out

para :: Functor f => (f (Term f, a) -> a) -> Term f -> a
para f = f . fmap fanout . out
  where fanout a = (a, para f a)


instance Eq1 Expression where
  eq1 = (==)

instance Show Term' where
  showsPrec = showsLevelPrec False

showsLevelPrec :: Bool -> Int -> Term' -> ShowS
showsLevelPrec isType n term = case out term of
  Variable name -> shows name
  Type -> showString "Type"
  Implicit -> showString "Implicit"
  Application a b -> showParen (n > prec) (showsLevelPrec isType prec a . showString " " . showsLevelPrec isType (prec + 1) b)
    where prec = 10
  Lambda i t body | Set.member (Local i) (freeVariables body) -> showString "λ " . shows (Local i) . showString " : " . showsLevel isType t  . showString " . " . showsLevel isType body
  Lambda _ t body -> showParen (n > 0) $ if isType
    then showsLevelPrec isType 1 t . showString " → " . showsLevel isType body
    else showString "λ _ : " . showsLevel isType t  . showString " . " . showsLevel isType body

showsLevel :: Bool -> Term' -> ShowS
showsLevel level = showsLevelPrec level 0

instance Eq1 f => Eq (Term f) where
  a == b = out a `eq1` out b

instance Eq1 f => Eq (Unification f) where
  Unification a == Unification b = a `eq1` b
  Conflict a1 b1 == Conflict a2 b2 = a1 == a2 && b1 == b2
  _ == _ = False

instance Show Unification' where
  show (Unification out) = show out
  show (Conflict a b) = "Expected: " ++ show a ++ "\n"
                     ++ "  Actual: " ++ show b ++ "\n"
