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
         gens = iterate iterInsertions insertion_map
         idx_gens = zip [0..] gens
         gen_10 = idx_gens !! 10
        --  gen_10_char_count = charCounts (snd gen_10)

        --  gen_10_max_count = maximum (map snd (Map.toList gen_10_char_count))
        --  gen_10_min_count = minimum (map snd (Map.toList gen_10_char_count))
     
     print template
     putStrLn "\n[Insertions]"
    --  mapM_ print insertion_rules

     mapM_ printInsertionGen (take 3 idx_gens)
    --  mapM_ printIdxGen (take 5 idx_gens)
    --  print (idx_gens !! 10)
    --  print (gen_10_max_count - gen_10_min_count)
  where 
    printIdxGen :: (Int, String) -> IO ()
    printIdxGen (idx, poly) = 
      do putStrLn $ "Gen " ++ show idx ++ ":"
         putStrLn poly

    printInsertionGen :: (Int, Insertions) -> IO ()
    printInsertionGen (gen, inserts) = 
      do putStrLn $ "[Inserts " ++ show gen ++ "]"
         mapM_ (\e -> putStrLn $ fst e ++ ": " ++ snd e) . Map.toList $ inserts
         
iterInsertions :: Insertions -> Insertions 
iterInsertions inserts = Map.mapWithKey iterInsertions inserts
  where 
    iterInsertions :: String -> String -> String
    iterInsertions [beg, end] middle = 
      tail $ resolveInsertions inserts ([beg] ++ middle ++ [end])
    iterInsertions _ _ = undefined

resolveInsertions :: Insertions -> String -> String 
resolveInsertions _ [] = []
resolveInsertions _ [_] = [] -- drop the last one
resolveInsertions inserts (c1:rest@(c2:cs)) = 
  case Map.lookup [c1,c2] inserts of 
    Nothing     -> c1 : resolveInsertions inserts rest
    Just insert -> [c1] ++ insert ++ resolveInsertions inserts rest


charCounts :: String -> Map Char Int
charCounts chars = Map.fromListWith (+) (zip chars (repeat 1))


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
