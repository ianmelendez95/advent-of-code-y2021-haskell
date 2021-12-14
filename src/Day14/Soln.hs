{-# LANGUAGE OverloadedStrings #-}

module Day14.Soln where 

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


inputFile :: FilePath
inputFile = "src/Day14/short-input.txt"


soln :: IO ()
soln = 
  do (template, insertion_rules) <- parseInput <$> TIO.readFile inputFile
     
     print template
     putStrLn "\n[Insertions]"
     mapM_ print insertion_rules


parseInput :: T.Text -> (String, [(String, String)])
parseInput input = 
  let input_lines = T.lines input
      template = T.unpack (head input_lines)
      insertion_rules = map parseInsertionRule (drop 2 input_lines)
   in (template, insertion_rules)
  where 
    parseInsertionRule :: T.Text -> (String, String)
    parseInsertionRule line =  
      let [base, _, repl] = map T.unpack (T.words line)
       in (base, repl)
