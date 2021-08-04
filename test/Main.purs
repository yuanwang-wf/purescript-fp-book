module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Main ( Age(..), FullName(..), Occupation(..), Person(..),  fromCSV, toCSV)

main :: Effect Unit
main = log $ show $ (toCSV person # fromCSV) == Just person
  where person = Person
                 { name: FullName "Sue Smith"
                 , age: Age 23
                 , occupation: Doctor}
