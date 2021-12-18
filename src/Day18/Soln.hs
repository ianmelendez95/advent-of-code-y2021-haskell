{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day18.Soln where 

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

type Range = (Int,Int)

type Parser = Parsec Void T.Text


data Tree = TNode Tree Tree
          | TInt  Int

instance Show Tree where 
  show (TInt v) = show v
  show (TNode v1 v2) = "[" ++ show v1 ++ "," ++ show v2 ++ "]"


inputFile = "src/Day18/short1-input.txt"


soln :: IO ()
soln = 
  do trees <- parseInput <$> TIO.readFile inputFile
     let depths = map treeDepth trees
         max_depth = maximum depths

     putStrLn "\n(Trees)"
     mapM_ print trees

     putStrLn "\n(Initial Concat)"
    --  print $ concatTrees trees

     putStrLn "\n(Depths)"
     putStrLn $ "Max: " ++ show max_depth
    --  mapM_ print depths


--------------------------------------------------------------------------------
-- Trees

appendTrees :: Tree -> Tree -> Tree
appendTrees = TNode

concatTrees :: [Tree] -> Tree
concatTrees = foldl1' appendTrees

treeDepth :: Tree -> Int
treeDepth (TInt t) = 0
treeDepth (TNode t1 t2) = 1 + max (treeDepth t1) (treeDepth t2)



--------------------------------------------------------------------------------
-- Parser

parseInput :: T.Text -> [Tree]
parseInput = map parseTree . T.lines

parseTree :: T.Text -> Tree
parseTree input = 
  case parse pTree "" input of 
    Left err -> error $ errorBundlePretty err
    Right r -> r

pTree :: Parser Tree
pTree = makeExprParser pTreeTerm operatorTable

pTreeInt :: Parser Tree
pTreeInt = TInt <$> L.decimal

pTreeTerm :: Parser Tree
pTreeTerm = 
  choice [ brackets pTree, pTreeInt ]

operatorTable :: [[Operator Parser Tree]]
operatorTable = 
  [[ InfixL (TNode <$ char ',') ]]

brackets :: Parser a -> Parser a
brackets = between (char '[') (char ']')
