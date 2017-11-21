{-# LANGUAGE OverloadedLists, OverloadedStrings #-}

module Main (main) where

import Boggle.Solver (solve) where

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
main = puts . show $ solve 3 board3x3 dictionary
