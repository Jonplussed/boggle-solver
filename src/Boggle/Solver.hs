{-# LANGUAGE OverloadedLists #-}

module Boggle.Solver where

import Data.Bool (bool)
import Data.Monoid ((<>))

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V

type Board = V.Vector Char
type Used = Set Int
type Results = Set T.Text

data Tree a = Branch a [Tree a] | Leaf a deriving (Eq, Show)
data State = State Int Used Results deriving (Eq, Show)

exampleBoard3x3 :: Board
exampleBoard3x3 =
  ['c','a','t'
  ,'r','a','t'
  ,'s','b','n'
  ]

nxnNeighbors :: Int -> Int -> [Int]
nxnNeighbors nxn at = top <> bottom <> left <> right
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

solve :: Int -> Board -> Results
solve nxn board = go $ State 0 [] []
