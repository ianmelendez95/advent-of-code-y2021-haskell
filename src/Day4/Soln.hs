{-# LANGUAGE OverloadedStrings #-}

module Day4.Soln where 

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR


import Data.List (intercalate)

type Board = [[Int]]


inputFile :: FilePath
inputFile = "src/Day4/short-input.txt"


soln :: IO ()
soln = 
  do content <- TIO.readFile inputFile
     let (draws, boards) = parseInput content
     print draws
     putStrLn ""
     putStrLn $ intercalate "\n" (map showBoard boards)


showBoard :: Board -> String
showBoard = unlines . map show

parseInput :: T.Text -> ([Int], [Board])
parseInput input = 
  let (draws, input') = parseDraws (T.lines input)
   in (draws, parseBoards input')
  where 
    parseDraws :: [T.Text] -> ([Int], [T.Text])
    parseDraws (ds:_:rest) = (readDraws ds, rest)
    parseDraws ls = error $ "Expecting initial draws: " ++ T.unpack (T.unlines ls)

    parseBoards :: [T.Text] -> [Board]
    parseBoards [] = []
    parseBoards lines = 
      let (board, lines') = parseBoard lines
       in board : parseBoards lines'

    parseBoard :: [T.Text] -> (Board, [T.Text])
    parseBoard input = 
      let (board_lines, input') = splitAt 5 input
          board = map readBoardRow board_lines
       in case input' of 
            []     -> (board, [])
            (_:ls) -> (board, ls)

readDraws :: T.Text -> [Int]
readDraws = map (read . T.unpack) . T.splitOn ","

readBoardRow :: T.Text -> [Int]
readBoardRow = map (read . T.unpack) . T.words