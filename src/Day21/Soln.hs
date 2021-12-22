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
inputFile = "src/Day20/full-input.txt"


soln :: IO ()
soln = 
  do print (take 5 (roundPositions (4, 8)))


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

