{-# LANGUAGE OverloadedStrings #-}

module Day6.Soln where 

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR

import Data.Ord (comparing)
import Data.List (intercalate, transpose, sort, isSubsequenceOf, find, (\\), maximumBy)
import Data.Maybe (listToMaybe, catMaybes)
import Control.Arrow (Arrow(first))

import qualified Data.Map.Strict as Map


inputFile :: FilePath
inputFile = "src/Day6/full-input.txt"


soln :: IO ()
soln = 
  do content <- TIO.readFile inputFile
     let init_fish = parseFish content
         fish_gens = zip [0..] (iterate iterFish init_fish)

     mapM_ printGen (take 19 fish_gens)
     putStrLn "..."
     putStrLn $ "Day 80: " ++ show (length (snd (fish_gens !! 80)))
  where 
    printGen :: (Int, [Int]) -> IO ()
    printGen (d, f) = 
      putStrLn $ "Day " ++ show d ++ ": " ++ show (length f) ++ " " ++ show f



iterFish :: [Int] -> [Int]
iterFish = go 0
  where 
    go :: Int -> [Int] -> [Int]
    go n_new [] = replicate n_new 8
    go n_new (f:fs)
      | f <= 0 = 6 : go (n_new + 1) fs
      | otherwise = f - 1 : go n_new fs


parseFish :: T.Text -> [Int]
parseFish = map (read . T.unpack) . T.splitOn ","