module Main where

import Prelude
import Data.Array (filter)

import Effect (Effect)
import Effect.Console (log)


even :: Int -> Boolean
even i = mod i  2 == 0

main :: Effect Unit
main = do
  (log <<< show <<< filter even) [1,2,3]
  log "🍝"
