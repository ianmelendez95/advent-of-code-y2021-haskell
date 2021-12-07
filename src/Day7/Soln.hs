{-# LANGUAGE OverloadedStrings #-}

module Day7.Soln where 

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR

import Data.Ord (comparing)
import Data.List (foldl1', intercalate, transpose, sort, isSubsequenceOf, find, (\\), maximumBy)
import Data.Maybe (listToMaybe, catMaybes)
import Control.Arrow (Arrow(first))

import qualified Data.Map.Strict as Map
import qualified Data.IntMap as IntMap

import Control.Monad.State.Lazy


inputFile :: FilePath
inputFile = "src/Day7/full-input.txt"


soln :: IO ()
soln = 
  do crabLocs <- readCrabLocs <$> TIO.readFile inputFile
     let rng = range crabLocs
         dists = map (rangeToDist rng) crabLocs

         tot_dist = foldl1' (zipWith (+)) dists


    --  putStrLn $ "Locs:    " ++ show crabLocs
    --  putStrLn $ "Dists:\n"  ++ unlines (map show dists)
    --  putStrLn $ "Tot:     " ++ show tot_dist
     putStrLn $ "Minimum: " ++ show (minimum tot_dist)


rangeToDist :: [Int] -> Int -> [Int]
rangeToDist range pivot = map (\i -> abs (i - pivot)) range


range :: [Int] -> [Int]
range xs = [minimum xs .. maximum xs]


readCrabLocs :: T.Text -> [Int]
readCrabLocs = map (read . T.unpack) . T.splitOn ","
