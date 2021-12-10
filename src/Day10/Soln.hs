{-# LANGUAGE OverloadedStrings #-}

module Day10.Soln where 

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR

import Data.Ord (comparing)
import Data.List (foldl1', intercalate, transpose, sort, isSubsequenceOf, find, (\\), maximumBy, permutations)
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


data CPart = Push CChar
           | Pop  CChar
           deriving Show

data CChar = Paren 
           | Brack
           | Curly
           | Angle
           deriving (Show, Eq)


inputFile = "src/Day10/full-input.txt"


soln :: IO ()
soln = 
  do parts <- parseInput <$> TIO.readFile inputFile
     let m_inc_chars = map findIncompleteChars parts
         inc_points = map incompletePoints $ catMaybes m_inc_chars

    --  mapM_ print parts
     mapM_ print m_inc_chars
     putStrLn $ "Points: " ++ show (median inc_points)

median :: [Int] -> Int
median xs = sort xs !! (length xs `div` 2)

findIncompleteChars :: [CPart] -> Maybe [CChar]
findIncompleteChars parts = 
  case go [] parts of 
    Left _ -> Nothing
    Right cs -> Just cs
  where 
    go :: [CChar] -> [CPart] -> Either CChar [CChar]
    go [] [] = Right []
    go stack [] = Right stack
    go stack (Push c : parts) = go (c : stack) parts
    go (sc : stack) (Pop c : parts) 
      | c /= sc = Left c
      | otherwise = go stack parts
    go [] ps@(Pop c : parts) = error $ "Can't pop empty stack: " ++ show ps


incompletePoints :: [CChar] -> Int
incompletePoints = go 0 . map charIncompletePoints
  where 
    go = foldl (\ps p -> (5 * ps) + p)

charIncompletePoints :: CChar -> Int
charIncompletePoints Paren = 1
charIncompletePoints Brack = 2
charIncompletePoints Curly = 3
charIncompletePoints Angle = 4

illegalPoints :: CChar -> Int
illegalPoints Paren = 3
illegalPoints Brack = 57
illegalPoints Curly = 1197
illegalPoints Angle = 25137


parseInput :: T.Text -> [[CPart]]
parseInput input = map parseLine $ T.lines input
  where 
    parseLine :: T.Text -> [CPart]
    parseLine = map parseChar . T.unpack

    parseChar :: Char -> CPart
    parseChar '(' = Push Paren
    parseChar '[' = Push Brack
    parseChar '{' = Push Curly
    parseChar '<' = Push Angle
    parseChar ')' = Pop  Paren
    parseChar ']' = Pop  Brack
    parseChar '}' = Pop  Curly
    parseChar '>' = Pop  Angle
    parseChar c = error $ "Unrecognized char: " ++ [c]
