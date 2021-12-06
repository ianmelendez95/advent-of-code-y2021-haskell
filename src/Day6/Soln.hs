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
inputFile = "src/Day6/short-input.txt"


soln :: IO ()
soln = 
  do content <- TIO.readFile inputFile
     let init_fish = parseFish content
     putStrLn $ "Fish: " ++ show init_fish


parseFish :: T.Text -> [Int]
parseFish = map (read . T.unpack) . T.splitOn ","