{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wall #-}

{-
This is a version of the exercises using OverloadedLabels and Generics instead
of calling makeLenses.
-}

module LabelsExercises where

import GHC.Generics (Generic)
import Optics.Core

{-
Here are some example data types from the slides, and some test data to
experiment with.
-}

data Person = MkPerson  {  name  :: String
                        ,  age   :: Int
                        ,  pets  :: [Pet] }
  deriving (Generic, Show)

data Pet = MkPet  {  name   :: String
                  ,  age    :: Int }
  deriving (Generic, Show)

alice, bob :: Person
alice = MkPerson { name = "Alice", age = 65, pets = [] }
bob = MkPerson { name = "Bob"
               , age  = 42
               , pets = [ MkPet { name = "Mr Scruffy", age = 3 } ]
               }


{-
The 'ages' fold extracts all the ages containined within a 'Person'.

 * Define a function that tests whether any of the ages exceed the given
   integer:

     anyOlderThan :: Int -> Person -> Bool

   (Avoid producing an intermediate list. The 'Optics.Fold' module has many
   useful eliminators for folds.)

 * Define the same function using 'has', which tests whether a fold returns any
   values.  Hint: check out 'filtered :: (a -> Bool) -> AffineFold a a'.
-}

ages :: Fold Person Int
ages = #age `summing` (#pets % folded % #age)

anyOlderThan :: Int -> Person -> Bool
anyOlderThan = undefined


{-
 * Define a `Fold Person Pet` that visits every Pet whose age is greater than
   the given integer:

     petsOlderThan :: Int -> Fold Person Pet

 * Use it to define a function that returns the list of names of such pets.

     petNamesOlderThan :: Int -> Person -> [String]

 * Why is 'filtered' a fold rather than a traversal?  Hint: read the Haddocks
   for 'unsafeFiltered'.
-}

petsOlderThan :: Int -> Fold Person Pet
petsOlderThan = undefined

petNamesOlderThan :: Int -> Person -> [String]
petNamesOlderThan = undefined



{-
 * Define a traversal that visits the name of a person and all the names of
   their pets:

     names :: Traversal' Person String

 * Use it to implement a function that capitalises all the names (using
   'Data.Char.toUpper'):

     capitaliseNames :: Person -> Person
-}

names :: Traversal' Person String
names = undefined

capitaliseNames :: Person -> Person
capitaliseNames = undefined


{-
 * Define a function that takes a list of 'Person's, each for each name that
   each of them contain (including pets' names), print out the name and read a
   replacement value from standard input:

    replaceNames :: [Person] -> IO [Person]

  For example:

    ghci> replaceNames [alice,bob]
    Replacement for Alice: Charlie
    Replacement for Bob: Bob
    Replacement for Mr Scruffy: Tiddles
    [MkPerson {_personName = "Charlie", _personAge = 65, _personPets = []},MkPerson {_personName = "Bob", _personAge = 42, _personPets = [MkPet {_petName = "Tiddles", _petAge = 3}]}]
-}

replaceNames :: [Person] -> IO [Person]
replaceNames = undefined
