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
traceMsgId msg x = traceMsgShow msg x x 

traceMsgShow :: (Show a) => String -> a -> b -> b
traceMsgShow msg x = trace (msg ++ ": " ++ show x)

type Range = (Int,Int)

type Parser = Parsec Void T.Text


data Tree = TNode Tree Tree
          | TInt  Int

type Expl = (Int,Int)

instance Show Tree where 
  show (TInt v) = show v
  show (TNode v1 v2) = "[" ++ show v1 ++ "," ++ show v2 ++ "]"


inputFile = "src/Day18/short4-input.txt"


soln :: IO ()
soln = 
  do trees <- parseInput <$> TIO.readFile inputFile
     let depths = map treeDepth trees
         max_depth = maximum depths

    --  putStrLn "\n(Trees)"
    --  mapM_ print trees

    --  putStrLn "\n(Initial Concat)"
    --  print $ concatTrees trees

    --  putStrLn "\n(Depths)"
    --  putStrLn $ "Max: " ++ show max_depth
    --  mapM_ print depths

     putStrLn "\n(Concat)"
     print $ concatTrees (take 2 trees)


--------------------------------------------------------------------------------
-- Tests

explTest1 :: IO ()
explTest1 = 
  do let t = parseTree "[[[[[1,1],[2,2]],[3,3]],[4,4]],[5,5]]"
     print t

     let t' = reduceTree t
     print t'


--------------------------------------------------------------------------------
-- Trees

appendTrees :: Tree -> Tree -> (Tree, Expl)
appendTrees tl tr = reduceTree (TNode tl tr)

concatTrees :: [Tree] -> Tree
concatTrees [] = error "no trees"
concatTrees (tree:trees) = fst $ foldl' doFold (tree, 0) trees
  where 
    doFold :: (Tree, Int) -> Tree -> (Tree, Int)
    doFold (tl, er) tr = 
      let (t', (_, er')) = reduceTree' (0, er') (TNode tl tr)
       in (t', er')

treeDepth :: Tree -> Int
treeDepth (TInt t) = 0
treeDepth (TNode t1 t2) = 1 + max (treeDepth t1) (treeDepth t2)

reduceTree :: Tree -> (Tree, Expl)
reduceTree = reduceTree' (0,0)

reduceTree' :: Expl -> Tree -> (Tree, Expl)
reduceTree' expl tree =
  let (tree', expl') = traceMsgId "explode" $ explodeTree' 0 expl (traceMsgId "reduce " tree)
   in case traceMsgId "split  " $ splitTree tree' of 
        Left tree'' -> ( tree'', expl')
        Right tree'' -> addExpl expl' <$> reduceTree' (0,0) tree''


explodeTree :: Tree -> (Tree, Expl)
explodeTree = explodeTree' 0 (0,0)

explodeTree' :: Int -> Expl -> Tree -> (Tree, Expl) 

explodeTree' cur_depth (el, er) t@(TInt v) = (TInt (v + el + er), (0,0))

explodeTree' cur_depth (el, er) t@(TNode tl tr)
  | cur_depth > 4 = error $ "Depth > 4 shouldn't be possible: " ++ show t
  | cur_depth == 4 = (TInt 0, intPair t)
  | otherwise = 
    let (tl', (tlel, tler)) = 
          explodeTree' (cur_depth + 1) (0, el) tl
        (tr', (trel, trer)) = 
          explodeTree' (cur_depth + 1) (er, 0) tr
        
        t' = TNode tl' tr'
      in if tler == 0 && trel == 0
           then (t', (tlel, trer))
           else if trel > 0
                       -- exp <- (so they combine and go left)
                  then addExpl ((tlel, trer))
                         <$> explodeTree' cur_depth (tler + trel, 0) (t')
                       -- exp ->
                  else addExpl ((tlel, trer))
                         <$> explodeTree' cur_depth (0, tler + trel) (t')
                        
-- | splits the tree, returning left is all splits
-- | handled safely, or right if there was a workplace
-- | accident (resulting explosion)
splitTree :: Tree -> Either Tree Tree
splitTree = go 0
  where 
    go :: Int -> Tree -> Either Tree Tree
    go cur_depth t@(TInt v) 
      | v < 10 = Left t
      -- | cur_depth > 4 = error $ "Depth > 4 shouldn't be possible: " ++ show t
      | cur_depth < 4 = go cur_depth (splitInt v)
      | otherwise     = Right $ splitInt v

    go cur_depth (TNode tl tr) =
      case go (cur_depth + 1) tl of 
        Left tl' -> mapEither (TNode tl') (go (cur_depth + 1) tr)
        Right tl' -> Right (TNode tl' tr)

mapEither :: (Tree -> Tree) -> Either Tree Tree -> Either Tree Tree
mapEither f (Left t) = Left (f t)
mapEither f (Right t) = Right (f t)


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
