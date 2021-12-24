{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}

module Day24.Soln where 

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

import Text.Megaparsec hiding (Pos (..), PosState (..))
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
inputFile = "src/Day24/full-input.txt"

soln :: IO ()
soln = 
  do instrs <- parseInput <$> TIO.readFile inputFile
     mapM_ print instrs


--------------------------------------------------------------------------------
-- Types

data Instr = Inp Var
           | Add Var Opand
           | Mul Var Opand
           | Div Var Opand
           | Mod Var Opand
           | Eql Var Opand

data Opand = OVar Var
           | OInt Int

data Var = X | Y | Z | W

instance Show Instr where 
  show (Inp var)    = unwords ["inp", show var]
  show (Add var op) = unwords ["add", show var, show op]
  show (Mul var op) = unwords ["mul", show var, show op]
  show (Div var op) = unwords ["div", show var, show op]
  show (Mod var op) = unwords ["mod", show var, show op]
  show (Eql var op) = unwords ["eql", show var, show op]

instance Show Opand where 
  show (OVar v) = show v
  show (OInt i) = show i

instance Show Var where 
  show X = "x"
  show Y = "y"
  show Z = "z"
  show W = "w"



--------------------------------------------------------------------------------
-- Parser

parseInput :: T.Text -> [Instr]
parseInput input = 
  case parse instructions "" input of 
    Left e -> error (errorBundlePretty e)
    Right r -> r

instructions :: Parser [Instr]
instructions = sepBy1 instruction eol

instruction :: Parser Instr
instruction = choice 
  [ Inp <$> (symbol "inp" >> var)
  , Add <$> (symbol "add" >> lexeme var) <*> opand
  , Mul <$> (symbol "mul" >> lexeme var) <*> opand
  , Div <$> (symbol "div" >> lexeme var) <*> opand
  , Mod <$> (symbol "mod" >> lexeme var) <*> opand
  , Eql <$> (symbol "eql" >> lexeme var) <*> opand
  ]

opand :: Parser Opand
opand = (OVar <$> var) <|> (OInt <$> int) 

int :: Parser Int
int = L.signed space L.decimal

var :: Parser Var
var = choice
  [ X <$ char 'x'
  , Y <$ char 'y'
  , Z <$ char 'z'
  , W <$ char 'w'
  ]


symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

sc :: Parser ()
sc = L.space space1 empty empty
