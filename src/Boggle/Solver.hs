{-# LANGUAGE OverloadedLists #-}

module Boggle.Solver where

import Data.Bool (bool)
import Data.Monoid ((<>))

import Data.Vector

data Tree a
  = Branch a [Tree a]
  | Leaf a
  deriving (Eq, Show)

neighbors :: Int -> Int -> [Int]
neighbors nxn at = top <> bottom <> left <> right
  where
    unless x = bool [x] []

    atTop = at < nxn
    atBottom = at >= nxn * pred nxn
    atLeft = mod at nxn == 0
    atRight = mod (succ at) nxn == 0

    top = (at - nxn) `unless` atTop
    bottom = (at + nxn) `unless` atBottom
    left = (pred at) `unless` atLeft
    right = (succ at) `unless` atRight

exampleBoard3x3 :: Vector Char
exampleBoard3x3 =
  ['c','a','t'
  ,'r','a','t'
  ,'s','b','n'
  ]
