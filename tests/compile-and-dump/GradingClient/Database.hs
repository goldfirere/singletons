{- Database.hs

(c) Richard Eisenberg 2012
eir@cis.upenn.edu

This file contains the full code for the database interface example
presented in /Dependently typed programming with singletons/

-}

{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances, ConstraintKinds, CPP #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

-- The OverlappingInstances is needed only to allow the InC and SubsetC classes.
-- This is simply a convenience so that GHC can infer the necessary proofs of
-- schema inclusion. The library could easily be designed without this flag,
-- but it would require a client to explicity build proof terms from
-- InProof and Subset.

module GradingClient.Database where

import Prelude hiding ( tail, id )
import Data.Singletons.Prelude
import Data.Singletons.SuppressUnusedWarnings
import Data.Singletons.TH
import Control.Monad
import Data.List hiding ( tail )

#ifdef MODERN_MTL
import Control.Monad.Except  ( throwError )
#else
import Control.Monad.Error   ( throwError )
#endif

$(singletons [d|
  -- Basic Nat type
  data Nat = Zero | Succ Nat deriving (Eq, Ord)
  |])

-- Conversions to any from Integers
fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Succ n) = (fromNat n) + 1

toNat :: Integer -> Nat
toNat 0         = Zero
toNat n | n > 0 = Succ (toNat (n - 1))
toNat _         = error "Converting negative to Nat"

-- Display and read Nats using decimal digits
instance Show Nat where
  show = show . fromNat
instance Read Nat where
  readsPrec n s = map (\(a,rest) -> (toNat a,rest)) $ readsPrec n s

$(singletons [d|
  -- Our "U"niverse of types. These types can be stored in our database.
  data U = BOOL
         | STRING
         | NAT
         | VEC U Nat deriving (Read, Eq, Show)

  -- A re-definition of Char as an algebraic data type.
  -- This is necessary to allow for promotion and type-level Strings.
  data AChar = CA | CB | CC | CD | CE | CF | CG | CH | CI
             | CJ | CK | CL | CM | CN | CO | CP | CQ | CR
             | CS | CT | CU | CV | CW | CX | CY | CZ
    deriving (Read, Show, Eq)

  -- A named attribute in our database
  data Attribute = Attr [AChar] U

  -- A schema is an ordered list of named attributes
  data Schema = Sch [Attribute]

  -- append two schemas
  append :: Schema -> Schema -> Schema
  append (Sch s1) (Sch s2) = Sch (s1 ++ s2)

  -- predicate to check that a schema is free of a certain attribute
  attrNotIn :: Attribute -> Schema -> Bool
  attrNotIn _ (Sch []) = True
  attrNotIn (Attr name u) (Sch ((Attr name' _) : t)) =
    (name /= name') && (attrNotIn (Attr name u) (Sch t))

  -- predicate to check that two schemas are disjoint
  disjoint :: Schema -> Schema -> Bool
  disjoint (Sch []) _ = True
  disjoint (Sch (h : t)) s = (attrNotIn h s) && (disjoint (Sch t) s)

  -- predicate to check if a name occurs in a schema
  occurs :: [AChar] -> Schema -> Bool
  occurs _ (Sch []) = False
  occurs name (Sch ((Attr name' _) : attrs)) =
    name == name' || occurs name (Sch attrs)

  -- looks up an element type from a schema
  lookup :: [AChar] -> Schema -> U
  lookup _ (Sch []) = undefined
  lookup name (Sch ((Attr name' u) : attrs)) =
    if name == name' then u else lookup name (Sch attrs)
  |])

-- The El type family gives us the type associated with a constructor
-- of U:
type family El (u :: U) :: *
type instance El BOOL = Bool
type instance El STRING = String
type instance El NAT  = Nat
type instance El (VEC u n) = Vec (El u) n

-- Length-indexed vectors
data Vec :: * -> Nat -> * where
  VNil :: Vec a Zero
  VCons :: a -> Vec a n -> Vec a (Succ n)

-- Read instances are keyed by the index of the vector to aid in parsing
instance Read (Vec a Zero) where
  readsPrec _ s = [(VNil, s)]
instance (Read a, Read (Vec a n)) => Read (Vec a (Succ n)) where
  readsPrec n s = do
    (a, rest) <- readsPrec n s
    (tail, restrest) <- readsPrec n rest
    return (VCons a tail, restrest)

-- Because the Read instances are keyed by the length of the vector,
-- it is not obvious to the compiler that all Vecs have a Read instance.
-- We must make a short inductive proof of this fact.

-- First, we define a datatype to store the resulting instance, keyed
-- by the parameters to Vec:
data VecReadInstance a n where
  VecReadInstance :: Read (Vec a n) => VecReadInstance a n

-- Then, we make a function that produces an instance of Read for a
-- Vec, given the datatype it is over and its length, both encoded
-- using singleton types:
vecReadInstance :: Read (El u) => SU u -> SNat n -> VecReadInstance (El u) n
vecReadInstance _ SZero = VecReadInstance
vecReadInstance u (SSucc n) = case vecReadInstance u n of
  VecReadInstance -> VecReadInstance

-- The Show instance can be straightforwardly defined:
instance Show a => Show (Vec a n) where
  show VNil = ""
  show (VCons h t) = (show h) ++ " " ++ (show t)

-- We need to be able to Read and Show elements of our database, so
-- we must know that any type of the form (El u) for some (u :: U)
-- has a Read and Show instance. Because we can't declare this instance
-- directly (as, in general, declaring an instance of a type family
-- would be unsound), we provide inductive proofs that these instances
-- exist:
data ElUReadInstance u where
  ElUReadInstance :: Read (El u) => ElUReadInstance u

elUReadInstance :: Sing u -> ElUReadInstance u
elUReadInstance SBOOL = ElUReadInstance
elUReadInstance SSTRING = ElUReadInstance
elUReadInstance SNAT  = ElUReadInstance
elUReadInstance (SVEC u n) = case elUReadInstance u of
  ElUReadInstance -> case vecReadInstance u n of
    VecReadInstance -> ElUReadInstance

data ElUShowInstance u where
  ElUShowInstance :: Show (El u) => ElUShowInstance u

elUShowInstance :: Sing u -> ElUShowInstance u
elUShowInstance SBOOL = ElUShowInstance
elUShowInstance SSTRING = ElUShowInstance
elUShowInstance SNAT  = ElUShowInstance
elUShowInstance (SVEC u _) = case elUShowInstance u of
  ElUShowInstance -> ElUShowInstance

showAttrProof :: Sing (Attr nm u) -> ElUShowInstance u
showAttrProof (SAttr _ u) = elUShowInstance u

-- A Row is one row of our database table, keyed by its schema.
data Row :: Schema -> * where
  EmptyRow :: [Int] -> Row (Sch '[]) -- the Ints are the unique id of the row
  ConsRow :: El u -> Row (Sch s) -> Row (Sch ((Attr name u) ': s))

-- We build Show instances for a Row element by element:
instance Show (Row (Sch '[])) where
  show (EmptyRow n) = "(id=" ++ (show n) ++ ")"
instance (Show (El u), Show (Row (Sch attrs))) =>
           Show (Row (Sch ((Attr name u) ': attrs))) where
  show (ConsRow h t) = case t of
        EmptyRow n -> (show h) ++ " (id=" ++ (show n) ++ ")"
        _ -> (show h) ++ ", " ++ (show t)

-- A Handle in our system is an abstract handle to a loaded table.
-- The constructor is not exported. In our simplistic case, we
-- just store the list of rows. A more sophisticated implementation
-- could store some identifier to the connection to an external database.
data Handle :: Schema -> * where
  Handle :: [Row s] -> Handle s

-- The following functions parse our very simple flat file database format.

-- The file, with a name ending in ".dat", consists of a sequence of lines,
-- where each line contains one entry in the table. There is no row separator;
-- if a row contains n pieces of data, that row is represented in n lines in
-- the file.

-- A schema is stored in a file of the same name, except ending in ".schema".
-- Each line in the file is a constructor of U indicating the type of the
-- corresponding row element.

-- Use Either for error handling in parsing functions
type ErrorM = Either String

-- This function is relatively uninteresting except for its use of
-- pattern matching to introduce the instances of Read and Show for
-- elements
readRow :: Int -> SSchema s -> [String] -> ErrorM (Row s, [String])
readRow id (SSch SNil) strs =
  return (EmptyRow [id], strs)
readRow _ (SSch (SCons _ _)) [] =
  throwError "Ran out of data while processing row"
readRow id (SSch (SCons (SAttr _ u) at)) (sh:st) = do
  (rowTail, strTail) <- readRow id (SSch at) st
  case elUReadInstance u of
    ElUReadInstance ->
      let results = readsPrec 0 sh in
      if null results
        then throwError $ "No parse of " ++ sh ++ " as a " ++
                          (show (fromSing u))
        else
          let item = fst $ head results in
          case elUShowInstance u of
            ElUShowInstance -> return (ConsRow item rowTail, strTail)

readRows :: SSchema s -> [String] -> [Row s] -> ErrorM [Row s]
readRows _ [] soFar = return soFar
readRows sch lst soFar = do
  (row, rest) <- readRow (length soFar) sch lst
  readRows sch rest (row : soFar)

-- Given the name of a database and its schema, return a handle to the
-- database.
connect :: String -> SSchema s -> IO (Handle s)
connect name schema = do
  schString <- readFile (name ++ ".schema")
  let schEntries = lines schString
      usFound = map read schEntries -- load schema just using "read"
      (Sch attrs) = fromSing schema
      usExpected = map (\(Attr _ u) -> u) attrs
  unless (usFound == usExpected) -- compare found schema with expected
    (fail "Expected schema does not match found schema")
  dataString <- readFile (name ++ ".dat")
  let dataEntries = lines dataString
      result = readRows schema dataEntries [] -- read actual data
  case result of
    Left errorMsg -> fail errorMsg
    Right rows -> return $ Handle rows

-- In order to define strongly-typed projection from a row, we need to have a notion
-- that one schema is a subset of another. We permit the schemas to have their columns
-- in different orders. We define this subset relation via two inductively defined
-- propositions. In Haskell, these inductively defined propositions take the form of
-- GADTs. In their original form, they would look like this:
{-
data InProof :: Attribute -> Schema -> * where
  InElt :: InProof attr (Sch (attr ': schTail))
  InTail :: InProof attr (Sch attrs) -> InProof attr (Sch (a ': attrs))

data SubsetProof :: Schema -> Schema -> * where
  SubsetEmpty :: SubsetProof (Sch '[]) s'
  SubsetCons :: InProof attr s' -> SubsetProof (Sch attrs) s' ->
                  SubsetProof (Sch (attr ': attrs)) s'
-}
-- However, it would be convenient to users of the database library not to require
-- building these proofs manually. So, we define type classes so that the compiler
-- builds the proofs automatically. To make everything work well together, we also
-- make the parameters to the proof GADT constructors implicit -- i.e. in the form
-- of type class constraints.

data InProof :: Attribute -> Schema -> * where
  InElt :: InProof attr (Sch (attr ': schTail))
  InTail :: InC name u (Sch attrs) => InProof (Attr name u) (Sch (a ': attrs))

class InC (name :: [AChar]) (u :: U) (sch :: Schema) where
  inProof :: InProof (Attr name u) sch
instance InC name u (Sch ((Attr name u) ': schTail)) where
  inProof = InElt
instance InC name u (Sch attrs) => InC name u (Sch (a ': attrs)) where
  inProof = InTail

data SubsetProof :: Schema -> Schema -> * where
  SubsetEmpty :: SubsetProof (Sch '[]) s'
  SubsetCons :: (InC name u s', SubsetC (Sch attrs) s') =>
                  SubsetProof (Sch ((Attr name u) ': attrs)) s'

class SubsetC (s :: Schema) (s' :: Schema) where
  subset :: SubsetProof s s'

instance SubsetC (Sch '[]) s' where
  subset = SubsetEmpty
instance (InC name u s', SubsetC (Sch attrs) s') =>
           SubsetC (Sch ((Attr name u) ': attrs)) s' where
  subset = SubsetCons

-- To access the data in a structured (and well-typed!) way, we use
-- an RA (short for Relational Algebra). An RA is indexed by the schema
-- of the data it produces.
data RA :: Schema -> * where
  -- The RA includes all data represented by the handle.
  Read :: Handle s -> RA s

  -- The RA is a union of the rows represented by the two RAs provided.
  -- Note that the schemas of the two RAs must be the same for this
  -- constructor use to type-check.
  Union :: RA s -> RA s -> RA s

  -- The RA is the list of rows in the first RA, omitting those in the
  -- second. Once again, the schemas must match.
  Diff :: RA s -> RA s -> RA s

  -- The RA is a Cartesian product of the two RAs provided. Note that
  -- the schemas of the two provided RAs must be disjoint.
  Product :: (Disjoint s s' ~ True, SingI s, SingI s') =>
               RA s -> RA s' -> RA (Append s s')

  -- The RA is a projection conforming to the schema provided. The
  -- type-checker ensures that this schema is a subset of the data
  -- included in the provided RA.
  Project :: (SubsetC s' s, SingI s) =>
               SSchema s' -> RA s -> RA s'

  -- The RA contains only those rows of the provided RA for which
  -- the provided expression evaluates to True. Note that the
  -- schema of the provided RA and the resultant RA are the same
  -- because the columns of data are the same. Also note that
  -- the expression must return a Bool for this to type-check.
  Select :: Expr s BOOL -> RA s -> RA s

-- Other constructors would be added in a more robust database
-- implementation.

-- An Expr is used with the Select constructor to choose some
-- subset of rows from a table. Expressions are indexed by the
-- schema over which they operate and the return value they
-- produce.
data Expr :: Schema -> U -> * where
  -- Equality among two elements
  Equal :: Eq (El u) => Expr s u -> Expr s u -> Expr s BOOL

  -- A less-than comparison among two Nats
  LessThan :: Expr s NAT -> Expr s NAT -> Expr s BOOL

  -- A literal number
  LiteralNat :: Integer -> Expr s NAT

  -- Projection in an expression -- evaluates to the value
  -- of the named attribute.
  Element :: (Occurs nm s ~ True) =>
               SSchema s -> Sing nm -> Expr s (Lookup nm s)

  -- A more robust implementation would include more constructors

-- Retrieves the id from a row. Ids are used when computing unions and
-- differences.
getId :: Row s -> [Int]
getId (EmptyRow n) = n
getId (ConsRow _ t) = getId t

-- Changes the id of a row to a new value
changeId :: [Int] -> Row s -> Row s
changeId n (EmptyRow _) = EmptyRow n
changeId n (ConsRow h t) = ConsRow h (changeId n t)

-- Equality for rows based on ids.
eqRow :: Row s -> Row s -> Bool
eqRow r1 r2 = getId r1 == getId r2

-- Equality for attributes based on names
eqAttr :: Attribute -> Attribute -> Bool
eqAttr (Attr nm _) (Attr nm' _) = nm == nm'

-- Appends two rows. There are three suspicious case statements -- they are
-- suspicious in that the different branches are all exactly identical. Here
-- is why they are needed:

-- The two case statements on r are necessary to deconstruct the index in the
-- type of r; GHC does not use the fact that s' must be (Sch a') for some a'.
-- By doing a case analysis on r, GHC uses the types given in the different
-- constructors for Row, both of which give the form of s' as (Sch a'). This
-- deconstruction is necessary for the type family Append to compute, because
-- Append is defined only when its second argument is of the form (Sch a').

-- The case statement on rowAppend t r is necessary to avoid potential
-- overlapping instances for the SingRep class; the instances are needed for
-- the call to ConsRow. The potential for overlapping instances comes from
-- ambiguity in the component types of (Append s s'). By doing case analysis
-- on rowAppend t r, these variables become fixed, and the potential for
-- overlapping instances disappears.

-- We use the "cases" Singletons library operation to produce the case
-- analysis in the first clause. This "cases" operation produces a case
-- statement where each branch is identical and each constructor parameter
-- is ignored. The "cases" operation does not work for the second clause
-- because the code in the clause depends on definitions generated earlier.
-- Template Haskell restricts certain dependencies between auto-generated
-- code blocks to prevent the possibility of circular dependencies.
-- In this case, if the $(singletons ...) blocks above were in a different
-- module, the "cases" operation would be applicable here.

$( return [] )

rowAppend :: Row s -> Row s' -> Row (Append s s')
rowAppend (EmptyRow n) r = $(cases ''Row [| r |]
                                   [| changeId (n ++ (getId r)) r |])
rowAppend (ConsRow h t) r = case r of
  EmptyRow _ ->
    case rowAppend t r of
      EmptyRow _ -> ConsRow h (rowAppend t r)
      ConsRow _ _ -> ConsRow h (rowAppend t r)
  ConsRow _ _ ->
    case rowAppend t r of
      EmptyRow _ -> ConsRow h (rowAppend t r)
      ConsRow _ _ -> ConsRow h (rowAppend t r)

-- Choose the elements of one list based on truth values in another
choose :: [Bool] -> [a] -> [a]
choose [] _ = []
choose (False : btail) (_ : t) = choose btail t
choose (True : btail) (h : t) = h : (choose btail t)
choose _ [] = []

-- The query function is the eliminator for an RA. It returns a list of
-- rows containing the data produced by the RA.
query :: forall s. SingI s => RA s -> IO [Row s]
query (Read (Handle rows)) = return rows
query (Union ra rb) = do
  rowsa <- query ra
  rowsb <- query rb
  return $ unionBy eqRow rowsa rowsb
query (Diff ra rb) = do
  rowsa <- query ra
  rowsb <- query rb
  return $ deleteFirstsBy eqRow rowsa rowsb
query (Product ra rb) = do
  rowsa <- query ra
  rowsb <- query rb
  return $ do -- entering the [] Monad
    rowa <- rowsa
    rowb <- rowsb
    return $ rowAppend rowa rowb
query (Project sch ra) = do
  rows <- query ra
  return $ map (projectRow sch) rows
  where -- The projectRow function uses the relationship encoded in the Subset
        -- relation to project the requested columns of data in a type-safe manner.

        -- It recurs on the structure of the provided schema, creating the output
        -- row to be in the same order as the input schema. This is necessary for
        -- the output to type-check, as it is indexed by the input schema.

        -- We use explicit quantification to get access to scoped type variables.
        projectRow :: forall (sch :: Schema) (s' :: Schema).
                        SubsetC sch s' => SSchema sch -> Row s' -> Row sch

        -- Base case: empty schema
        projectRow (SSch SNil) r = EmptyRow (getId r)

        -- In the recursive case, we need to pattern-match on the proof that
        -- the provided schema is a subset of the provided RA. We extract this
        -- proof (of type SubsetProof s s') from the SubsetC instance using the
        -- subset method.
        projectRow (SSch (SCons attr tail)) r =
          case subset :: SubsetProof sch s' of

            -- Because we know that the schema is non-empty, the only possibility
            -- here is SubsetCons:
            SubsetCons ->
              let rtail = projectRow (SSch tail) r in
                case attr of
                  SAttr _ u -> case elUShowInstance u of
                    ElUShowInstance -> ConsRow (extractElt attr r) rtail

            -- GHC correctly determines that this case is impossible if it is
            -- not commented.
            -- SubsetEmpty -> undefined <== IMPOSSIBLE

            -- However, the current version of GHC (7.5) does not suppress warnings
            -- for incomplete pattern matches when the remaining cases are impossible.
            -- So, we include this case (impossible to reach for any terminated value)
            -- to suppress the warning.
            _ -> error "Type checking failed"

        -- Retrieves the element, looked up by the name of the provided attribute,
        -- from a row. The explicit quantification is necessary to create the scoped
        -- type variables to use in the return type of <<inProof>>
        extractElt :: forall nm u sch. InC nm u sch =>
                        Sing (Attr nm u) -> Row sch -> El u
        extractElt attr r = case inProof :: InProof (Attr nm u) sch of
          InElt -> case r of
            ConsRow h _ -> h
            -- EmptyRow _ -> undefined <== IMPOSSIBLE
            _ -> error "Type checking failed"
          InTail  -> case r of
            ConsRow _ t -> extractElt attr t
            -- EmptyRow _ -> undefined <== IMPOSSBLE
            _ -> error "Type checking failed"

query (Select expr r) = do
  rows <- query r
  let vals = map (eval expr) rows
  return $ choose vals rows
  where -- Evaluates an expression
        eval :: forall s' u. SingI s' => Expr s' u -> Row s' -> El u
        eval (Element _ (name :: Sing name)) row =
          case row of
            -- EmptyRow _ -> undefined <== IMPOSSIBLE
            ConsRow h t -> case row of
              (ConsRow _ _ :: Row (Sch ((Attr name' u') ': attrs))) ->
                case sing :: Sing s' of
                  -- SSch SNil -> undefined <== IMPOSSIBLE
                  SSch (SCons (SAttr name' _) stail) ->
                    case name %:== name' of
                      STrue -> h
                      SFalse -> withSingI stail (eval (Element (SSch stail) name) t)
                  _ -> bugInGHC
            _ -> bugInGHC

        eval (Equal (e1 :: Expr s' u') e2) row =
          let v1 = eval e1 row
              v2 = eval e2 row in
          v1 == v2

        -- Note that the types really help us here: the LessThan constructor is
        -- defined only over Expr s NAT, so we know that evaluating e1 and e2 will
        -- yield Nats, which are a member of the Ord type class.
        eval (LessThan e1 e2) row =
          let v1 = eval e1 row
              v2 = eval e2 row in
          v1 < v2

        eval (LiteralNat x) _ = toNat x
