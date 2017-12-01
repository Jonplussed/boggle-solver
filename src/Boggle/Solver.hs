{-# LANGUAGE OverloadedLists #-}

module Boggle.Solver where

import Data.Bool (bool)
import Data.Monoid ((<>))
import Data.Set ((\\))
import Data.Vector ((!))

import qualified Data.Foldable as F
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
  , found :: Results
  } deriving (Eq, Show)

-- Crappy first iteration. This could be redone with foldl', no conversions
-- between list/set, in parallel, and possibly stack-safely.
solve :: Int -> Board -> Dictionary -> Results
solve nxn board dict =
    sFoldMap (go emptyState) . S.toList $ nxnCells nxn
  where
    go :: State -> Int -> Results
    go state index
        | L.null neighbors = found'
        | otherwise = sFoldMap (go state') neighbors
      where
        neighbors = S.toList $ nxnNeighbors nxn index \\ used'

        used' = S.insert index $ used state
        word' = T.snoc (word state) (board ! index)
        found' = bool (found state) (S.insert word' $ found state) (S.member word' dict)
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

-- Because a Set is spine-strict, we prefer our own strict version of "foldMap"
sFoldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
sFoldMap f = F.foldl' (\acc x -> (f x) <> acc) mempty
