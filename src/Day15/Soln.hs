{-# LANGUAGE OverloadedStrings #-}

module Day15.Soln where 

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR

import Data.Ord (comparing)
import Data.List
import Data.Maybe (maybe, fromMaybe, listToMaybe, catMaybes, mapMaybe)
import Data.Char

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Set (Set)
import qualified Data.Set as Set

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Control.Monad.State.Lazy

import Debug.Trace


type Point = (Int, Int)
type PointMap a = Map.Map Point a


inputFile = "src/Day15/short-input.txt"


soln :: IO ()
soln = 
  do point_map <- parseInput <$> TIO.readFile inputFile
     printPointMap point_map
    


adjacentPoints :: Point -> [Point]
adjacentPoints (r,c) = 
  [ (r - 1, c)
  , (r + 1, c)
  , (r,     c - 1)
  , (r,     c + 1)
  ]

printPointMap :: PointMap Int -> IO ()
printPointMap levels = printEntries (Map.toList levels)
  where 
    printEntries :: [(Point, Int)] -> IO ()
    printEntries [] = pure () 
    printEntries [(_,l)] = print l
    printEntries (((m,n),l):rest@(((m',_),_):_))
      | m' > m = print l >> printEntries rest
      | otherwise = putStr (show l) >> printEntries rest

parseInput :: T.Text -> PointMap Int
parseInput input = Map.fromList . concat $ zipWith lineToPoints [0..] (T.lines input)
  where
    lineToPoints :: Int -> T.Text -> [(Point, Int)]
    lineToPoints row_idx line = 
      zipWith (\col_idx height -> ((row_idx, col_idx), height)) [0..] (rowHeights line)

    rowHeights :: T.Text -> [Int]
    rowHeights line = map digitToInt (T.unpack line)
