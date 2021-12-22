{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}

module Day21.Soln where 

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR

import Data.Ord
import Data.List
import Data.Maybe
import Data.Char

import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Set (Set)
import qualified Data.Set as Set

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Control.Monad.State.Lazy

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void

import Debug.Trace
import Control.Monad.Combinators.Expr 

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

traceMsgId :: (Show a) => String -> a -> a
traceMsgId msg x = traceMsgShow msg x x 

traceMsgShow :: (Show a) => String -> a -> b -> b
traceMsgShow msg x = trace (msg ++ ": " ++ show x)

type Parser = Parsec Void T.Text


inputFile :: FilePath
inputFile = "src/Day21/short-input.txt"


soln :: IO ()
soln = 
  do init_pos@(p1,p2) <- parseInput <$> TIO.readFile inputFile

     putStrLn "\n[Initial Positions]"
     print (p1,p2)

     let positions = roundPositions init_pos
         scores = cumSumPairs (drop 1 positions)
         idx_scores = zip [0..] scores
        
         (Just win_round) = find (isWinRound . snd) idx_scores

     putStrLn "\n[Round Positions]"
     mapM_ print (take 5 positions)
    
     putStrLn "\n[Round Scores]"
     mapM_ print (take 5 idx_scores)

     putStrLn "\n[Winning Round]"
     print win_round
     putStrLn $ "Rolls: " ++ show (winRoundRolls win_round)

    --  putStrLn "\n[Result]"
    --  print (winRoundRolls win_round * loserScore (snd win_round))
  where 
    isWinRound :: (Int,Int) -> Bool
    isWinRound (s1,s2) = s1 >= 1000 || s2 >= 1000

    winRoundRolls :: (Int,(Int,Int)) -> Int
    winRoundRolls r@(round,(s1, s2))
      | s1 >= 1000 = ((round - 1) * 6) + 3
      | s2 >= 1000 = ((round - 1) * 6) + 6
      | otherwise = error $ "No winner: " ++ show r

    loserScore :: (Int,Int) -> Int
    loserScore (s1, s2)
      | s1 <= s2 = s1
      | otherwise = s2
     

roundScores :: (Int,Int) -> [(Int,Int)]
roundScores = cumSumPairs . roundPositions

cumSumPairs :: [(Int,Int)] -> [(Int,Int)]
cumSumPairs = scanl' doSum (0,0)
  where 
    doSum :: (Int,Int) -> (Int,Int) -> (Int,Int)
    doSum (s1, s2) (x1, x2) = 
      let s1' = s1 + x1
          s2' = s2 + x2
       in s1' `seq` s2' `seq` (s1', s2')


roundPositions :: (Int,Int) -> [(Int,Int)]
roundPositions initial_pos = scanl' scanf initial_pos rollSums
  where 
    scanf :: (Int,Int) -> (Int,Int) -> (Int,Int)
    scanf (p1_pos, p2_pos) (p1_roll,  p2_roll) = 
      (moveRoll p1_roll p1_pos, moveRoll p2_roll p2_pos)
    
    moveRoll :: Int -> Int -> Int
    moveRoll roll pos = (((pos - 1) + roll) `mod` 10) + 1


rollSums :: [(Int,Int)]
rollSums = go [1..]
  where 
    go :: [Int] -> [(Int,Int)]
    go rolls = 
      let (p1_rolls, rolls')  = splitAt 3 rolls
          (p2_rolls, rolls'') = splitAt 3 rolls'
       in (sum p1_rolls, sum p2_rolls) : go rolls''


parseInput :: T.Text -> (Int,Int)
parseInput input = 
  let [p1,p2] = map parseLine (T.lines input)
   in (p1,p2)
  where 
    parseLine :: T.Text -> Int
    parseLine = read . T.unpack . last . T.words
