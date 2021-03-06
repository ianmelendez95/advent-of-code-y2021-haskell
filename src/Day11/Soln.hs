{-# LANGUAGE OverloadedStrings #-}

module Day11.Soln where 

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR

import Data.Ord (comparing)
import Data.List
import Data.Maybe (maybe, fromMaybe, listToMaybe, catMaybes, mapMaybe)
import Data.Char (digitToInt)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- import Data.IntMap (IntMap)
-- import qualified Data.IntMap as IntMap

import Data.Set (Set)
import qualified Data.Set as Set

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Control.Monad.State.Lazy

import Debug.Trace


type Point = (Int, Int)
type PointMap a = Map.Map Point a

type Dumbo = Int
type Dumbos = PointMap Int
type DumboS = State Dumbos

inputFile = "src/Day11/full-input.txt"


soln :: IO ()
soln = 
  do dumbos <- parseInput <$> TIO.readFile inputFile
     let dumbo_gens = iterate iterDumboGen (False, dumbos)
         idx_gens = zip [0..] dumbo_gens
    
     mapM_ printGen (take 4 idx_gens)
     putStrLn "\n...\n"
     printGen (fromMaybe undefined $ find isSyncedGen idx_gens)
  where 
    isSyncedGen :: (Int, (Bool, Dumbos)) -> Bool
    isSyncedGen = fst . snd

    printGen :: (Int, (Bool, Dumbos)) -> IO ()
    printGen (i, (fs, ds)) = 
      do putStrLn ""
         putStrLn $ "[Gen " ++ show i ++ "]"
         putStrLn $ "Synced: " ++ show fs
         printDumbos ds


--------------------------------------------------------------------------------
-- Dumbos State

iterDumboGen :: (Bool, Dumbos) -> (Bool, Dumbos)
iterDumboGen = iterDumbos . snd

iterDumbos :: Dumbos -> (Bool, Dumbos)
iterDumbos dumbos =
  let base_growth = iterBaseGrowth dumbos
   in resetFlashed $ execState (mapM_ (iterFlashes . fst) (Map.toList base_growth)) base_growth 
  
iterFlashes :: Point -> DumboS ()
iterFlashes p =
  do cur_level <- lookupPointM p
     if cur_level <= 9
       then pure ()
       else let adj_points = adjacentPoints p
             in do setPointM p (-1)
                   mapM_ incrPoint adj_points
                   mapM_ iterFlashes adj_points

lookupPointM :: Point -> DumboS Int
lookupPointM point = gets (fromMaybe (-1) . Map.lookup point)

setPointM :: Point -> Int -> DumboS ()
setPointM point level = modify (Map.insert point level)

incrPoint :: Point -> DumboS ()
incrPoint point = modify (Map.adjust (\l -> if l < 0 then l else l + 1) point) 


resetFlashed :: Dumbos -> (Bool, Dumbos)
resetFlashed dumbos = 
  let all_flashed = all ((<0) . snd) (Map.toList dumbos)
   in (all_flashed, Map.map (max 0) dumbos)

iterBaseGrowth :: Dumbos -> Dumbos
iterBaseGrowth = Map.map (+1)

adjacentPoints :: Point -> [Point]
adjacentPoints (r,c) = 
  [ (r - 1, c - 1)
  , (r - 1, c)
  , (r - 1, c + 1)
  , (r,     c - 1)
  , (r,     c + 1)
  , (r + 1, c - 1)
  , (r + 1, c)
  , (r + 1, c + 1)
  ]

     

printDumbos :: Dumbos -> IO ()
printDumbos levels = printEntries (Map.toList levels)
  where 
    printEntries :: [(Point, Int)] -> IO ()
    printEntries [] = pure () 
    printEntries [(_,l)] = print l
    printEntries (((m,n),l):rest@(((m',_),_):_))
      | m' > m = print l >> printEntries rest
      | otherwise = putStr (show l) >> printEntries rest

parseInput :: T.Text -> PointMap Int
parseInput input = Map.fromList . concat $ zipWith lineToPoints [0..] (T.lines input)
  where
    lineToPoints :: Int -> T.Text -> [(Point, Int)]
    lineToPoints row_idx line = 
      zipWith (\col_idx height -> ((row_idx, col_idx), height)) [0..] (rowHeights line)

    rowHeights :: T.Text -> [Int]
    rowHeights line = map digitToInt (T.unpack line)
