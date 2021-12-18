{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day16.Soln where 

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
import Control.Monad.Combinators.Expr
import Data.Void

import Debug.Trace

type Parser = Parsec Void T.Text

data Packet = PInt Int Int
            | POp  Int PType [Packet]
            deriving Show

           -- nary
data PType = PTSum
           | PTProd
           | PTMin
           | PTMax

           -- nullary
           | PTInt

           -- binary
           | PTGt
           | PTLt
           | PTEq

           deriving Show


inputFile = "src/Day16/full-input.txt"


soln :: IO ()
soln = 
  do packets <- parseInput inputFile
    --  print packets
    --  print packet

     mapM_ printPacketRes packets
  where 
    printPacketRes :: Packet -> IO ()
    printPacketRes p = 
      do putStrLn $ "\nVersion Sum: " ++ show (versionSum p)
         putStrLn $ "Eval: " ++ show (packetValue p)


versionSum :: Packet -> Int
versionSum (PInt v _) = v
versionSum (POp v _ ps) = v + sum (map versionSum ps)

packetValue :: Packet -> Int
packetValue (PInt _ v) = v
packetValue (POp _ op ps) = opValue op (map packetValue ps)
  where 
    opValue :: PType -> [Int] -> Int
    opValue PTInt  vs = error "PTInt is a separate packet type"
    opValue PTSum  vs = sum vs
    opValue PTProd vs = product vs
    opValue PTMin  vs = minimum vs
    opValue PTMax  vs = maximum vs
    opValue PTGt   vs = if all (head vs >)  (tail vs) then 1 else 0
    opValue PTLt   vs = if all (head vs <)  (tail vs) then 1 else 0
    opValue PTEq   vs = if all (head vs ==) (tail vs) then 1 else 0


--------------------------------------------------------------------------------
-- Parser

parseInput :: FilePath -> IO [Packet]
parseInput filepath = 
  map (parsePacket . T.concatMap hexToBin) . T.lines <$> TIO.readFile filepath
     
     
     

parsePacket :: T.Text -> Packet
parsePacket bin_str =
  do case parse (packet <* trailingZeros) "" bin_str of 
       Left err -> error (errorBundlePretty  err) 
       Right res -> res


--------------------------------------------------------------------------------
-- Parser Combinators

trailingZeros :: Parser ()
trailingZeros = takeWhileP (Just "trailing 0") (== '0') >> eof

packet :: Parser Packet
packet = 
  do v <- pVersion
     t <- pType
     case t of 
       PTInt -> PInt v   <$> packetIntValue
       _     -> POp  v t <$> subpackets


packetIntValue :: Parser Int
packetIntValue = readBin <$> packetIntValueBin

packetIntValueBin :: Parser T.Text
packetIntValueBin = 
  do sign <- anySingle
     val  <- takeP (Just "value digit") 4
     case sign of 
       '1' -> T.append val <$> packetIntValueBin
       '0' -> pure val
       c   -> error $ "Unkown value digit: " ++ [c]


subpackets :: Parser [Packet]
subpackets = 
  do sign <- anySingle
     case sign of 
       '0' -> subpacketsByLength
       '1' -> subpacketsByCount
       c   -> error $ "Unkown subpackets type digit: " ++ [c]


subpacketsByLength :: Parser [Packet]
subpacketsByLength = 
  do len <- readBin <$> takeP (Just "subpackets length digit") 15
     parseAllSubpackets <$> takeP (Just "subpackets content") len

parseAllSubpackets :: T.Text -> [Packet]
parseAllSubpackets txt = 
  case parse parseAll "inline" txt of 
    Left err -> error (errorBundlePretty err)
    Right res -> res
  where 
    parseAll :: Parser [Packet]
    parseAll = some packet <* eof


subpacketsByCount :: Parser [Packet]
subpacketsByCount = 
  do c <- readBin <$> takeP (Just "subpacket count") 11 
     count c packet
     


pVersion :: Parser Int
pVersion = readBin <$> takeP (Just "version digit") 3

pType :: Parser PType
pType = readType <$> takeP (Just "type digit") 3

readType :: T.Text -> PType
readType "000" = PTSum
readType "001" = PTProd
readType "010" = PTMin
readType "011" = PTMax
readType "100" = PTInt
readType "101" = PTGt
readType "110" = PTLt
readType "111" = PTEq
readType tipe = error $ "Unrecognized type: " ++ T.unpack tipe

readBin :: T.Text -> Int
readBin = T.foldl' (\tot x -> 2 * tot + digitToInt x) 0


--------------------------------------------------------------------------------
-- Low Level Parsing

-- parseInput :: T.Text -> T.Text
-- parseInput = T.concatMap hexToBin

hexToBin :: Char -> T.Text
hexToBin '0' = "0000"
hexToBin '1' = "0001"
hexToBin '2' = "0010"
hexToBin '3' = "0011"
hexToBin '4' = "0100"
hexToBin '5' = "0101"
hexToBin '6' = "0110"
hexToBin '7' = "0111"
hexToBin '8' = "1000"
hexToBin '9' = "1001"
hexToBin 'A' = "1010"
hexToBin 'B' = "1011"
hexToBin 'C' = "1100"
hexToBin 'D' = "1101"
hexToBin 'E' = "1110"
hexToBin 'F' = "1111"
hexToBin c = error $ "Invalid hex: " ++ [c]