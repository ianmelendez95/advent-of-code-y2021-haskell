{-# LANGUAGE OverloadedStrings #-}

module Day8.Soln where 

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
inputFile = "src/Day8/full-input.txt"


soln :: IO ()
soln = 
  do segs <- map parseSegments . T.lines <$> TIO.readFile inputFile
     let n_easy = sum $ map (countEasySegs . snd) segs
     putStrLn $ "Count: " ++ show n_easy


countEasySegs :: [T.Text] -> Int
countEasySegs = length . filter id . map isEasySeg

isEasySeg :: T.Text -> Bool
isEasySeg seg = T.length seg `elem` [2, 4, 3, 7]


parseSegments :: T.Text -> ([T.Text], [T.Text])
parseSegments line = 
  let [input_segs, output_segs] = map T.words (T.splitOn "|" line)
   in (input_segs, output_segs)