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


type Insertions = Map String String


inputFile :: FilePath
inputFile = "src/Day14/short-input.txt"


soln :: IO ()
soln = 
  do (template, insertion_rules) <- parseInput <$> TIO.readFile inputFile

     let insertion_map = Map.fromList insertion_rules
         gens = iterate (iterInsertions insertion_map) template
         idx_gens = zip [0..] gens
     
    --  print template
    --  putStrLn "\n[Insertions]"
    --  mapM_ print insertion_rules
     mapM_ printIdxGen (take 5 idx_gens)
    --  printIdxGen (idx_gens !! 10)
  where 
    printIdxGen :: (Int, String) -> IO ()
    printIdxGen (idx, poly) = 
      do putStrLn $ "Gen " ++ show idx ++ ":"
         putStrLn poly



iterInsertions :: Insertions -> String -> String 
iterInsertions _ [] = []
iterInsertions _ [c] = [c]
iterInsertions inserts (c1:rest@(c2:cs)) = 
  case Map.lookup [c1,c2] inserts of 
    Nothing     -> c1 : iterInsertions inserts rest
    Just insert -> [c1] ++ insert ++ iterInsertions inserts rest


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
