{-# LANGUAGE OverloadedLists #-}

module Boggle.Solver where

import Data.Bool (bool)
import Data.Monoid ((<>))
import Data.Set ((\\))
import Data.Vector ((!))

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V

type Board = V.Vector Char
type Dictionary = S.Set T.Text
type Results = S.Set T.Text

data State = State
  { used :: S.Set Int
  , word :: T.Text
  , found :: [T.Text]
  } deriving (Eq, Show)

-- Crappy first iteration. This could be redone with foldl', no conversions
-- between list/set, in parallel, and possibly stack-safely.
solve :: Int -> Board -> Dictionary -> Results
solve nxn board dict =
    S.fromList . concatMap (go emptyState) . S.toList $ nxnCells nxn
  where
    go :: State -> Int -> [T.Text]
    go state index
        | L.null neighbors = found'
        | otherwise = concatMap (go state') neighbors
      where
        neighbors = S.toList $ nxnNeighbors nxn index \\ used'

        used' = S.insert index $ used state
        word' = T.snoc (word state) (board ! index)
        found' = bool (found state) (word' : found state) (S.member word' dict)
        state' = State used' word' found'

-- internal functions

nxnCells :: Int -> S.Set Int
nxnCells nxn = [0 .. pred (nxn * nxn)]

nxnNeighbors :: Int -> Int -> S.Set Int
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

emptyState :: State
emptyState =
  State
    { used = mempty
    , word = mempty
    , found = mempty
    }
