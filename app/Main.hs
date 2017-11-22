{-# LANGUAGE OverloadedLists, OverloadedStrings #-}

module Main where

import Boggle.Solver (solve)
import Data.Set (Set)
import Data.Text (Text)
import Data.Vector (Vector)

board2x2 :: Vector Char
board2x2 =
  ['c','a'
  ,'z','t'
  ]

board3x3 :: Vector Char
board3x3 =
  ['c','a','t'
  ,'r','a','t'
  ,'s','b','n'
  ]

dictionary :: Set Text
dictionary =
  [ "a"
  , "at"
  , "bar"
  , "bat"
  , "cat"
  , "crab"
  , "crabs"
  , "rat"
  , "tabs"
  , "tac"
  , "tar"
  ]

main :: IO ()
main = putStr . show $ solve 3 board3x3 dictionary
