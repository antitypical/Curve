{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, FlexibleInstances #-}
module Curve where

import Data.Functor.Classes
import qualified Data.List as List
import qualified Data.Set as Set

data Name
  = Local Int
  | Global String
  deriving (Eq, Ord)

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


-- DSL for constructing terms

type' :: Term'
type' = Term Type

implicit :: Term'
implicit = Term Implicit

variable :: Name -> Term'
variable = Term . Variable

local :: Int -> Term'
local = variable . Local

global :: String -> Term'
global = variable . Global

infixl 9 `apply`

apply :: Term' -> Term' -> Term'
apply a = Term . Application a

infixr `lambda`

lambda :: Term' -> (Term' -> Term') -> Term'
lambda t f = Term $ Lambda i t body
  where i = maybe 0 succ $ maxBoundVariable body
        body = f (Term $ Variable $ Local i)

infixr -->

(-->) :: Term' -> Term' -> Term'
a --> b = a `lambda` const b

infixr `pi`

pi :: Term' -> (Term' -> Term') -> Term'
pi = lambda


-- Unifications

into :: Functor f => Term f -> Unification f
into term = Unification $ into <$> out term

unified :: Unification' -> Maybe Term'
unified (Unification expression) = Term <$> traverse unified expression
unified (Conflict _ _) = Nothing


-- Binding

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

maxBoundVariable :: Term' -> Maybe Int
maxBoundVariable = cata (\ expression -> case expression of
  Lambda n t _ -> max (Just n) t
  Application a b -> max a b
  _ -> Nothing)


-- Recursion schemes

cata :: Functor f => (f a -> a) -> Term f -> a
cata f = f . fmap (cata f) . out

para :: Functor f => (f (Term f, a) -> a) -> Term f -> a
para f = f . fmap fanout . out
  where fanout a = (a, para f a)


-- Numerals

digits :: Integral a => a -> a -> [a]
digits base i = fst $ foldr nextDigit ([], i) (replicate (fromIntegral $ countDigits base i) ())
  where nextDigit _ (list, prev) | (next, remainder) <- prev `divMod` base = (remainder : list, next)

countDigits :: Integral a => a -> a -> a
countDigits base i = 1 + floor (logBase (fromIntegral base) (fromIntegral $ abs i) :: Double)

showNumeral :: Integral i => String -> i -> String
showNumeral "" _ = ""
showNumeral alphabet i = List.genericIndex alphabet <$> digits (List.genericLength alphabet) i


-- Instances

instance Show Name where
  show (Local i) = showNumeral ['a'..'z'] i
  show (Global s) = s

instance Eq1 Expression where
  eq1 = (==)

instance Show Term' where
  showsPrec = showsLevelPrec False

showsLevelPrec :: Bool -> Int -> Term' -> ShowS
showsLevelPrec isType n term = case out term of
  Variable name -> shows name
  Type -> showString "Type"
  Implicit -> showString "_"
  Application a b -> showParen (n > prec) (showsLevelPrec isType prec a . showString " " . showsLevelPrec isType (prec + 1) b)
    where prec = 10
  Lambda i t body | Set.member (Local i) (freeVariables body) -> if isType
    then showString "(" . shows (Local i) . showString " : " . showsType t . showString ") → " . showsType body
    else showString "λ " . shows (Local i) . showString " : " . showsType t  . showString " . " . showsLevel isType body
  Lambda _ t body -> showParen (n > 0) $ if isType
    then showsLevelPrec True 1 t . showString " → " . showsType body
    else showString "λ _ : " . showsType t  . showString " . " . showsLevel isType body

showsLevel :: Bool -> Term' -> ShowS
showsLevel level = showsLevelPrec level 0

showsType :: Term' -> ShowS
showsType = showsLevel True

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
