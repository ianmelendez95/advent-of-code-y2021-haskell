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
import qualified Data.IntMap as IntMap

import Control.Monad.State.Lazy


type Memo a = State (IntMap.IntMap Int) a

evalMemo :: Memo a -> a
evalMemo memo = evalState memo IntMap.empty

memoLookup :: Int -> Memo (Maybe Int)
memoLookup dt = gets (IntMap.lookup dt)


inputFile :: FilePath
inputFile = "src/Day6/full-input.txt"


soln :: IO ()
soln = 
  do content <- TIO.readFile inputFile
     let init_fish = parseFish content

     putStrLn $ "Day 80: " ++ show (fishGen 80 init_fish)
     putStrLn $ "Day 256: " ++ show (fishGen 256 init_fish)
  where 
    printGen :: (Int, [Int]) -> IO ()
    printGen (d, f) = 
      putStrLn $ "Day " ++ show d ++ ": " ++ show (length f) ++ " " ++ show f


fishGen :: Int -> [Int] -> Int
fishGen dt = sum . map (\i -> 1 + evalMemo (nAncestorsMemo (dt - i)))

nAncestorsMemo :: Int -> Memo Int
nAncestorsMemo dt 
  | dt <= 0 = pure 0
  | otherwise = 
    do existing <- gets (IntMap.lookup dt)
       case existing of 
         Just x -> pure x
         Nothing -> 
           do n7 <- nAncestorsMemo (dt - 7)
              n9 <- nAncestorsMemo (dt - 9)
              let value = 1 + n7 + n9
              modify (IntMap.insert dt value) 
              pure value

nAncestors :: Int -> Int
nAncestors dt
  | dt > 0 = 1 + nAncestors (dt - 7) + nAncestors (dt - 9)
  | otherwise = 0
  

-- nAncestorsWithInc :: Int -> Int -> Int
-- nAncestorsWithInc incubation dt = 
--   1 + nAncestors (max 0 (dt - (incubation + 1)))

-- nAncestors :: Int -> Int
-- nAncestors dt = nChildren dt + sum (map nAncestors (dtRange (dt - 7)))
--   where 
--     nChildren :: Int -> Int
--     nChildren dt = dt `div` 7

--     dtRange :: Int -> [Int]
--     dtRange dt
--       | dt <= 0 = []
--       | otherwise = dt : dtRange (dt - 7)

-- fresh adult = a fish with 6 days left
-- freshAdultChildren :: Int -> Int
-- freshAdultChildren days = days `div` 7

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