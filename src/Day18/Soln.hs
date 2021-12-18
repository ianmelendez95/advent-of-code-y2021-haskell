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

traceMsgId :: (Show a) => String -> a -> a
traceMsgId msg x = x -- traceMsgShow msg x x 

traceMsgShow :: (Show a) => String -> a -> b -> b
traceMsgShow msg x = id -- trace (msg ++ ": " ++ show x)

type Range = (Int,Int)

type Parser = Parsec Void T.Text


data Tree = TNode Tree Tree
          | TInt  Int

type Expl = (Int,Int)

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
-- Tests

explTest1 :: IO ()
explTest1 = 
  do let t = parseTree "[[[[1,1],[2,2]],[3,3]],[4,4]]"
     print t

     let t' = preReduceTree t
     print t'


--------------------------------------------------------------------------------
-- Trees

appendTrees :: Tree -> Tree -> Tree
appendTrees = TNode

concatTrees :: [Tree] -> Tree
concatTrees = foldl1' appendTrees

treeDepth :: Tree -> Int
treeDepth (TInt t) = 0
treeDepth (TNode t1 t2) = 1 + max (treeDepth t1) (treeDepth t2)

-- | Preemptively reduce tree, pending concatenation.
-- | In other words, reduce as if its depth was 1 more than it 
-- | is, since that is what it will have right after its concatenated
-- | with another tree.
preReduceTree :: Tree -> (Tree, Expl)
preReduceTree = preReduceTree' 0 (0,0)

preReduceTree' :: Int -> Expl -> Tree -> (Tree, Expl) 

preReduceTree' cur_depth (el, er) t@(TInt v) =
  let v' = v + el + er
    in if v' >= 10
        then preReduceTree' cur_depth (0,0) (splitInt v')
        else (TInt v', (0,0))

-- TODO: consider propagating explosions with special traversal
preReduceTree' cur_depth (el, er) t@(TNode tl tr)
  | cur_depth > 3 = error $ "Depth > 3 shouldn't be possible: " ++ show t
  | cur_depth == 3 = (TInt 0, intPair t)
  | otherwise = 
    let (tl', (tlel, tler)) = 
          preReduceTree' (cur_depth + 1) (0, el) tl
        (tr', (trel, trer)) = 
          preReduceTree' (cur_depth + 1) (er, 0) tr
        
        t' = TNode tl' tr'
      in if tler == 0 && trel == 0
           then (traceMsgId "no prop" t', (tlel, trer))
           else if trel > 0
                       -- exp <- (so they combine and go left)
                  then addExpl (traceMsgId "prop left" (tlel, trer))
                         <$> preReduceTree' cur_depth (tler + trel, 0) (traceMsgId "prop left" t')
                       -- exp ->
                  else addExpl (traceMsgId "prop right" (tlel, trer))
                         <$> preReduceTree' cur_depth (0, tler + trel) (traceMsgId "prop right" t')

explHasValue :: Expl -> Bool
explHasValue (e1, e2) = e1 > 0 || e2 > 0

addExpl :: Expl -> Expl -> Expl
addExpl (e1, e2) (e1', e2') = (e1 + e1', e2 + e2')
    
intPair :: Tree -> (Int,Int)
intPair (TNode (TInt i1) (TInt i2)) = (i1, i2)
intPair tree = error $ "Expecting number pair but got: " ++ show tree

splitInt :: Int -> Tree
splitInt v = TNode (TInt (floorDiv v 2)) (TInt (ceilDiv v 2))

floorDiv :: Int -> Int -> Int
floorDiv = div

ceilDiv :: Int -> Int -> Int
ceilDiv x y = (x + y - 1) `div` y


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
