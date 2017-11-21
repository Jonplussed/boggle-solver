{-# LANGUAGE OverloadedLists #-}

module Boggle.Solver where

import Data.Bool (bool)
import Data.Monoid ((<>))

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V

data State = State
  { index :: Int
  , used :: Set Int
  , acc :: Text
  , results :: Set T.Text
  } deriving (Eq, Show)

type Board = V.Vector Char
type Dictionary = Set T.Text

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

solve :: Int -> Board -> Dictionary -> Results
solve nxn board dict = results . go $ State 0 mempty mempty mempty
  where
    go st | isFinished st = st

    go st 
      where
        index' =
        used' = S.insert (index st) (used st)
        acc' = T.snoc (acc st) (board ! index st)
        results' = bool results (S.insert acc' results) (S.member acc' dict)

    neighbors = nxnNeighbors nxn
    isFinished st = succ index == nxn * nxn
