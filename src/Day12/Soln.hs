{-# LANGUAGE OverloadedStrings #-}

module Day12.Soln where 

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


type Graph = Map String [String]


inputFile :: FilePath
inputFile = "src/Day12/full-input.txt"


soln :: IO ()
soln = 
  do cave_graph <- parseInput <$> TIO.readFile inputFile
     let path_count = paths "start" cave_graph
     putStrLn $ "Paths: " ++ show path_count


paths :: String -> Graph -> Int
paths "end" _ = 1
paths cur_cave graph = 
  let graph' = if isSmallCave cur_cave 
                 then deleteCave cur_cave graph 
                 else graph
   in case Map.lookup cur_cave graph of 
        Nothing -> 0
        Just next_caves ->
          sum (map (`paths` graph') next_caves)

isSmallCave :: String -> Bool
isSmallCave [] = error "No cave name"
isSmallCave (c:_) = isLower c

deleteCave :: String -> Graph -> Graph
deleteCave cave graph = 
  Map.map (deleteAll cave) (Map.delete cave graph)
  where 
    deleteAll :: String -> [String] -> [String]
    deleteAll _ [] = []
    deleteAll x (y:ys)
      | x == y = deleteAll x ys
      | otherwise = y : deleteAll x ys


parseInput :: T.Text -> Graph
parseInput input = 
  let ls = T.lines input
   in Map.fromListWith (++) (concatMap parseEdge ls)
  where 
    parseEdge :: T.Text -> [(String, [String])]
    parseEdge line = 
      let [s,e] = map T.unpack $ T.splitOn "-" line
       in [(s, [e]), (e, [s])]


