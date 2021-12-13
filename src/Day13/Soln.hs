{-# LANGUAGE OverloadedStrings #-}

module Day13.Soln where 

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

data Fold = FoldX Int
          | FoldY Int
          deriving Show


inputFile :: FilePath
inputFile = "src/Day13/short-input.txt"

soln :: IO ()
soln = 
  do (points, folds) <- parseInput <$> TIO.readFile inputFile

     putStrLn "[Points]"
     mapM_ print points

     putStrLn "\n[Folds]"
     mapM_ print folds


parseInput :: T.Text -> ([Point], [Fold])
parseInput input = 
  let (points, folds) = break T.null (T.lines input)
   in (map parsePoint points, map parseFold (tail folds))
  where 
    parsePoint :: T.Text -> Point
    parsePoint point_line = 
      let [x,y] = map (read . T.unpack) (T.splitOn "," point_line)
       in (x,y)
    
    parseFold :: T.Text -> Fold
    parseFold fold_line = 
      let [comp, val] = T.splitOn "=" (last (T.words fold_line))
       in case comp of 
            "x" -> FoldX (read (T.unpack val))
            "y" -> FoldY (read (T.unpack val))
            c -> error $ "Unknown input: " ++ show c
        

