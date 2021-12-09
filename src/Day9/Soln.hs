{-# LANGUAGE OverloadedStrings #-}

module Day9.Soln where 

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR

import Data.Ord (comparing)
import Data.List (foldl1', intercalate, transpose, sort, isSubsequenceOf, find, (\\), maximumBy, permutations)
import Data.Maybe (fromMaybe, listToMaybe, catMaybes, mapMaybe)
import Data.Char (digitToInt)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- import Data.IntMap (IntMap)
-- import qualified Data.IntMap as IntMap

import Data.Set (Set)
import qualified Data.Set as Set

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Control.Monad.State.Lazy

import Debug.Trace

type Point = (Int,Int)


inputFile :: FilePath
inputFile = "src/Day9/full-input.txt"


soln :: IO ()
soln = 
  do points_to_height <- parseInput <$> TIO.readFile inputFile
     let low_points = mapMaybe (`pointIfLowPointEntry` points_to_height) (Map.toList points_to_height)
         basins = map (`pointToBasin` points_to_height) low_points
         largest_basins = take 3 . reverse . sort $ map length basins
     
    --  mapM_ (\basin -> putStrLn $ show (length basin) ++ ": " ++ show basin) basins
     print (product largest_basins)

     
pointToBasin :: Point -> Map.Map Point Int -> [Point]
pointToBasin point height_map = (Set.toList . Set.fromList) $ pointToBasin' point height_map

pointToBasin' :: Point -> Map.Map Point Int -> [Point]
pointToBasin' point height_map =
  let adjacent_basin_points = filter isBasinPoint (adjacentPoints point)
   in point : concatMap (`pointToBasin` Map.insert point 10 height_map) adjacent_basin_points
  where 
    isBasinPoint :: Point -> Bool
    isBasinPoint adj_point = 
      let adj_point_height = lookupHeight adj_point height_map
       in adj_point_height < 9 && adj_point_height >= point_height

    point_height = lookupHeight point height_map



pointIfLowPointEntry :: (Point, Int) -> Map.Map Point Int -> Maybe Point
pointIfLowPointEntry (point, point_height) height_map = 
  let adjacent_heights = map (`lookupHeight` height_map) (adjacentPoints point)
   in if all (> point_height) adjacent_heights
        then Just point
        else Nothing

heightIfLowPointEntry :: (Point, Int) -> Map.Map Point Int -> Maybe Int
heightIfLowPointEntry (point, point_height) height_map = 
  let adjacent_heights = map (`lookupHeight` height_map) (adjacentPoints point)
   in if all (> point_height) adjacent_heights
        then trace (show point ++ " " ++ show point_height) $ Just point_height
        else Nothing

adjacentPoints :: Point -> [Point]
adjacentPoints (r,c) = 
  [ (r - 1, c)
  , (r,     c - 1)
  , (r,     c + 1)
  , (r + 1, c)
  ]

lookupHeight :: Point -> Map.Map Point Int -> Int
lookupHeight point = fromMaybe 10 . Map.lookup point


parseInput :: T.Text -> Map.Map Point Int
parseInput input = Map.fromList . concat $ zipWith lineToPoints [0..] (T.lines input)
  where
    lineToPoints :: Int -> T.Text -> [(Point, Int)]
    lineToPoints line_idx line = 
      zipWith (\row_idx height -> ((line_idx, row_idx), height)) [0..] (rowHeights line)

    rowHeights :: T.Text -> [Int]
    rowHeights line = map digitToInt (T.unpack line)
