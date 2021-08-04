module Main where

import Prelude

import Data.Array (filter)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)


newtype CSV = CSV String
derive newtype instance showCSV :: Show CSV

class ToCSV a where
  toCSV :: a -> CSV

class FromCSV a where
  fromCSV :: CSV -> Maybe a

newtype FullName = FullName String
derive newtype instance showFullName :: Show FullName

newtype Age = Age Int
derive instance newtypeAge :: Newtype Age _
derive newtype instance showAge :: Show Age

data Occupation = Doctor | Dentist | Lawyer | Unemployed
derive instance genericOccupation :: Generic Occupation _

instance showOccupation :: Show Occupation where
  show = genericShow

data Person = Person
   { name :: FullName
   , age :: Age
   , occupation :: Occupation
   }

instance toCSVPerson :: ToCSV Person where
  toCSV (Person {name, age, occupation}) = CSV $ show name <> "," <> show age <> "," <> show occupation

even :: Int -> Boolean
even i = i `mod`  2 == 0

main :: Effect Unit
main = log $ show $ toCSV
  (Person { name : FullName "Sue Smite"
          , age : Age 23
          , occupation: Doctor})
