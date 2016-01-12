{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, FlexibleInstances, UndecidableInstances #-}
module Curve where

import Data.Functor.Classes
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map

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

type' :: Roll r => r Expression
type' = roll Type

implicit :: Roll r => r Expression
implicit = roll Implicit

variable :: Roll r => Name -> r Expression
variable = roll . Variable

local :: Roll r => Int -> r Expression
local = variable . Local

global :: Roll r => String -> r Expression
global = variable . Global

infixl 9 `apply`

apply :: Roll r => r Expression -> r Expression -> r Expression
apply a = roll . Application a

infixr `lambda`

lambda :: (Roll r, PartialUnroll r) => r Expression -> (r Expression -> r Expression) -> r Expression
lambda t f = roll $ Lambda i t body
  where i = maybe 0 succ $ maxBoundVariable body
        body = f (local i)

infixr -->

(-->) :: (Roll r, PartialUnroll r) => r Expression -> r Expression -> r Expression
a --> b = a `lambda` const b

infixr `pi`

pi :: Term' -> (Term' -> Term') -> Term'
pi = lambda


-- Unifications

into :: Functor f => Term f -> Unification f
into term = Unification $ into <$> out term

unified :: Unification' -> Maybe Term'
unified (Unification expression) = Term <$> traverse unified expression
unified _ = Nothing

expected :: Unification' -> Term'
expected (Unification out) = Term $ expected <$> out
expected (Conflict a _) = a
expected _ = implicit

actual :: Unification' -> Term'
actual (Unification out) = Term $ actual <$> out
actual (Conflict _ b) = b
actual _ = implicit


-- Binding

rename :: (Roll r, PartialUnroll r) => Name -> Name -> r Expression -> r Expression
rename old new term | old == new = term
rename old new term = case unrollMaybe term of
  Just (Variable name) | name == old -> variable new
  Just (Lambda name t b) -> roll $ if Local name == old
    then Lambda name (rename old new t) b
    else Lambda name (rename old new t) (rename old new b)
  Just (Application a b) -> rename old new a `apply` rename old new b
  _ -> term

substitute :: (Roll r, PartialUnroll r) => Int -> r Expression -> r Expression -> r Expression
substitute name withTerm inScope = case unrollMaybe inScope of
  Just (Variable (Local n)) | n == name -> withTerm
  Just (Lambda n inType inBody) -> if n == name
    then roll $ Lambda n (substitute name withTerm inType) inBody
    else roll $ Lambda n (substitute name withTerm inType) (substitute name withTerm inBody)
  Just (Application inA inB) -> roll $ Application (substitute name withTerm inA) (substitute name withTerm inB)
  _ -> inScope


freeVariables :: Term' -> Set.Set Name
freeVariables = cata $ \ expression -> case expression of
  Variable name -> Set.singleton name
  Lambda i t b -> Set.delete (Local i) b `Set.union` t
  Application a b -> a `Set.union` b
  _ -> mempty

maxBoundVariable :: PartialUnroll r => r Expression -> Maybe Int
maxBoundVariable = foldl maximal Nothing . unrollMaybe
  where maximal into (Lambda i t _) = max into $ max (Just i) (maxBoundVariable t)
        maximal into (Application a b) = max into $ max (maxBoundVariable a) (maxBoundVariable b)
        maximal into _ = into


alphaEquivalent :: Term' -> Term' -> Bool
alphaEquivalent a b | a == b = True
alphaEquivalent (Term (Variable _)) (Term (Variable _)) = True
alphaEquivalent (Term (Application a1 b1)) (Term (Application a2 b2)) = alphaEquivalent a1 a2 && alphaEquivalent b1 b2
alphaEquivalent (Term (Lambda _ t1 b1)) (Term (Lambda _ t2 b2)) = alphaEquivalent t1 t2 && alphaEquivalent b1 b2
alphaEquivalent _ _ = False


-- Naming

freshBy :: (Name -> Bool) -> Name -> Name
freshBy isUsed name = if isUsed name then freshBy isUsed (prime name) else name

freshIn :: Set.Set Name -> Name -> Name
freshIn names = freshBy (`Set.member` names)

prime :: Name -> Name
prime (Local i) = Local $ succ i
prime (Global s) = Global $ s ++ "ʹ"

pick :: Set.Set Name -> Name
pick names = freshIn names (maximum $ Set.insert (Local 0) names)


-- Typechecking

type Context = Map.Map Name Term'

infer :: Context -> Term' -> Unification'
infer = check implicit

check :: Term' -> Context -> Term' -> Unification'
check expected context term = case (out term, out expected) of
  (Type, Implicit) -> Unification Type

  (Variable name, Implicit) -> maybe (Conflict implicit implicit) into (Map.lookup name context)

  (Lambda i t body, Implicit) -> unify t type' `lambda` \ v -> substitute i v (into body)

  (_, Implicit) -> Conflict implicit implicit
  (_, _) -> let unification = infer context term in
    maybe unification (unify expected) $ unified unification


-- Equality

unify :: Term' -> Term' -> Unification'
unify expected actual = case (out expected, out actual) of
  (_, Implicit) -> into expected
  (Implicit, _) -> into actual

  (Type, Type) -> into expected

  (Variable _, Variable _) -> into expected
  (Application a1 b1, Application a2 b2) -> unify a1 a2 `apply` unify b1 b2
  (Lambda i a1 b1, Lambda _ a2 b2) -> Unification $ Lambda i (unify a1 a2) (unify b1 b2)

  _ -> Conflict expected actual


-- Numerals

digits :: Integral a => a -> a -> [a]
digits base i = fst $ foldr nextDigit ([], i) (replicate (fromIntegral $ countDigits base i) ())
  where nextDigit _ (list, prev) | (next, remainder) <- prev `divMod` base = (remainder : list, next)

countDigits :: Integral a => a -> a -> a
countDigits base i = 1 + floor (logBase (fromIntegral base) (fromIntegral $ abs i) :: Double)

showNumeral :: Integral i => String -> i -> String
showNumeral "" _ = ""
showNumeral alphabet i = List.genericIndex alphabet <$> digits (List.genericLength alphabet) i


-- Classes

class Roll g where
  roll :: f (g f) -> g f

class Unroll r where
  unroll :: r f -> f (r f)

class PartialUnroll r where
  unrollMaybe :: r f -> Maybe (f (r f))

class Catamorphable r where
  cata :: Functor f => (f a -> a) -> r f -> a

  para :: Functor f => (f (r f, a) -> a) -> r f -> a


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

instance Roll Term where
  roll = Term

instance Unroll Term where
  unroll = out

instance PartialUnroll Term where
  unrollMaybe = Just . unroll

instance Roll Unification where
  roll = Unification

instance Unroll r => Catamorphable r where
  cata f = f . fmap (cata f) . unroll

  para f = f . fmap fanout . unroll
    where fanout a = (a, para f a)

instance PartialUnroll Unification where
  unrollMaybe (Unification expression) = Just expression
  unrollMaybe _ = Nothing
