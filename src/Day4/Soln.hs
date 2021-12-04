{-# LANGUAGE OverloadedStrings #-}

module Day4.Soln where 

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR


import Data.List (intercalate, transpose, sort, isInfixOf, find)
import Data.Maybe (listToMaybe, catMaybes)


type Board = [[Int]]


inputFile :: FilePath
inputFile = "src/Day4/short-input.txt"


soln :: IO ()
soln = 
  do content <- TIO.readFile inputFile
     let (draws, boards) = parseInput content
         seqs_to_board = map (\b -> (boardToSortedSeqs b, b)) boards
         draw_seqs = map sort (drop 4 $ listToCumList draws)

         winning_boards = map (\draw_seq -> find (isWinningBoardEntry draw_seq) seqs_to_board) draw_seqs
         first_winning = snd . head $ catMaybes winning_boards


     putStrLn "[Setup]"
     print draws
     putStrLn ""
     putStrLn $ intercalate "\n" (map showBoard boards)
     putStrLn ""

     putStrLn "[Comparison]"
     --  putStrLn $ "Draw Seqs: " ++ show draw_seqs
     mapM_ printSeqsToBoard seqs_to_board


     putStrLn "[WINNER!]"
     putStrLn $ showBoard first_winning

    


  where 
    printSeqsToBoard :: ([[Int]], Board) -> IO ()
    printSeqsToBoard (sqs, b) =
      do putStrLn "[Seqs]"
         putStrLn $ showBoard sqs
         putStrLn "[Board]"
         putStrLn $ showBoard b

    isWinningBoardEntry :: [Int] -> ([[Int]], Board) -> Bool
    isWinningBoardEntry draw_seq (board_seqs, _) = any (`isInfixOf` draw_seq) board_seqs


-- [1,2,3] -> [[1], [1,2], [1,2,3]]
listToCumList :: [Int] -> [[Int]]
listToCumList = scanl1 (++) . map (:[])


boardToSortedSeqs :: Board -> [[Int]]
boardToSortedSeqs board = map sort (board ++ transpose board)


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