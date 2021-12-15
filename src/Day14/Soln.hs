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


type Insertions = Map String [String]
type PairCount = Map String Int
type CharCount = Map Char Int


inputFile :: FilePath
inputFile = "src/Day14/short-input.txt"


-- | iter => gen
-- | 0 => 0
-- | 1 => 2
-- | 2 => 4
-- | 3 => 8
-- | 4 => 16
-- | 5 => 32
-- | 6 => 64
soln :: IO ()
soln = 
  do (template, insertion_rules) <- parseInput <$> TIO.readFile inputFile

     let insertion_map = insertionsFromRules insertion_rules
         template_count_map = templatePairCount template

         count_iters = iterate (iterPairCount insertion_map) template_count_map
     
     print template
    --  putStrLn "\n[Insertions]"
    --  mapM_ print insertion_rules

    --  putStrLn "\n[Pair Counts]"
    --  mapM_ printEntry (Map.toList template_count_map)
     mapM_ printPairCounts (take 4 $ zip [0..] count_iters)

     printCharCounts (10, pairCountsToCharCounts (count_iters !! 10))

  where 
    printIdxGen :: (Int, String) -> IO ()
    printIdxGen (idx, poly) = 
      do putStrLn $ "Gen " ++ show idx ++ ":"
         putStrLn poly

    printPairCounts :: (Int, PairCount) -> IO ()
    printPairCounts (gen, counts) = 
      do putStrLn $ "[Counts " ++ show gen ++ "]"
         mapM_ (\e -> putStrLn $ fst e ++ ": " ++ show (snd e)) . Map.toList $ counts

    printCharCounts :: (Int, CharCount) -> IO ()
    printCharCounts (gen, counts) = 
      do putStrLn $ "[Counts " ++ show gen ++ "]"
         mapM_ (\e -> putStrLn $ [fst e] ++ ": " ++ show (snd e)) . Map.toList $ counts
    
    iterToGen :: Int -> Int
    iterToGen iter = 2 ^ iter

    printEntry :: (Show a, Show b) => (a, b) -> IO ()
    printEntry (k, v) = putStrLn $ show k ++ ": " ++ show v

pairCountsToCharCounts :: PairCount -> CharCount
pairCountsToCharCounts pair_counts = 
  Map.fromListWith (+) (concatMap pairToCharCount (Map.toList pair_counts))
  where 
    pairToCharCount :: (String, Int) -> [(Char, Int)]
    pairToCharCount (pair, count) = zip pair (repeat count)

iterPairCount :: Insertions -> PairCount -> PairCount
iterPairCount inserts count_map = 
  Map.fromListWith (+) (concatMap iterCountEntry (Map.toList count_map))
  where 
    iterCountEntry :: (String, Int) -> [(String, Int)]
    iterCountEntry (pair, num) = 
      case Map.lookup pair inserts of 
        Nothing -> error $ "No inserts for pair: " ++ pair
        Just ins -> zip ins (repeat num)

templatePairCount :: String -> PairCount
templatePairCount = Map.fromListWith (+) . flip zip (repeat 1) . pairs

insertionsFromRules :: [(String, String)] -> Insertions
insertionsFromRules = Map.fromList . map fromRule
  where 
    fromRule :: (String, String) -> (String, [String])
    fromRule (from, to) = 
      let [c1, c3] = from
          [c2] = to
       in (from, [[c1,c2], [c2,c3]])

pairs :: String -> [String]
pairs [] = []
pairs [_] = []
pairs (x:rest@(y:ys)) = [x,y] : pairs rest


-- charCounts :: String -> Map Char Int
-- charCounts chars = Map.fromListWith (+) (zip chars (repeat 1))


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
