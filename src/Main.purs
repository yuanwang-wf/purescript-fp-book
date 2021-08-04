module Main where

import Prelude

import Data.Array (filter)
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Maybe (Maybe (..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Console (log)


newtype CSV = CSV String
derive newtype instance showCSV :: Show CSV

class ToCSV a where
  toCSV :: a -> CSV

class FromCSV a where
  fromCSV :: CSV -> Maybe a

newtype FullName = FullName String
derive instance newtypeFullName :: Newtype FullName _
derive newtype instance showFullName :: Show FullName
derive newtype instance eqFullName :: Eq FullName

newtype Age = Age Int
derive instance newtypeAge :: Newtype Age _
derive newtype instance showAge :: Show Age
derive newtype instance egAge :: Eq Age

data Occupation = Doctor | Dentist | Lawyer | Unemployed
derive instance genericOccupation :: Generic Occupation _
derive instance eqOccupation :: Eq Occupation

instance showOccupation :: Show Occupation where
  show = genericShow

data Person = Person
   { name :: FullName
   , age :: Age
   , occupation :: Occupation
   }
derive instance eqPerson :: Eq Person

toOccupation :: String -> Maybe Occupation
toOccupation = case _ of
  "Doctor" -> Just Doctor
  "Dentist" -> Just Lawyer
  "Unemployed" -> Just Unemployed
  _ -> Nothing

instance toCSVPerson :: ToCSV Person where
  toCSV (Person {name, age, occupation}) = CSV $ show name <> "," <> show age <> "," <> show occupation

instance fromCSVPerson :: FromCSV Person where
  fromCSV (CSV str) = case split (Pattern ",") str of
    [name, age, occupation] -> do
      age' <- fromString age
      occ <- toOccupation occupation
      pure $ Person {
          name : FullName name
        , age: Age age'
        , occupation: occ}
    _ -> Nothing


even :: Int -> Boolean
even i = i `mod`  2 == 0

main :: Effect Unit
main = log $ show $ toCSV
  (Person { name : FullName "Sue Smite"
          , age : Age 23
          , occupation: Doctor})
