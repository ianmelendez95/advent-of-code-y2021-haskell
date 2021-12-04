{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}


module Day4.Soln where 

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR

import Data.Ord (comparing)
import Data.List (intercalate, transpose, sort, isSubsequenceOf, find, (\\), maximumBy)
import Data.Maybe (listToMaybe, catMaybes)
import Control.Arrow (Arrow(first))


type Board = [[Int]]


inputFile :: FilePath
inputFile = "src/Day4/full-input.txt"


soln :: IO ()
soln = 
  do  content <- TIO.readFile inputFile
      let (draws, boards) = parseInput content
          seqs_to_board = map (\b -> (boardToSortedSeqs b, b)) boards
          draw_seqs = map sort (drop 4 $ listToCumList draws)

          winning_boards = map (\draw_seq -> (draw_seq,) . snd <$> find (isWinningBoardEntry draw_seq) seqs_to_board) draw_seqs
          (win_draw_seq, win_board) = head $ catMaybes winning_boards

          win_sum = boardScore win_draw_seq win_board
          last_draw = draws !! (length win_draw_seq - 1)

          -- part 2

          rev_draw_seqs = reverse draw_seqs
          round_to_board = map (boardWinSeq draw_seqs) seqs_to_board
          (_, lose_draw_seq, lose_board) = maximumBy (comparing fst3) round_to_board

          lose_sum = boardScore lose_draw_seq lose_board
          last_lose_draw = draws !! (length lose_draw_seq - 1)

      putStrLn "[Setup]"
      print draws
      putStrLn ""
      putStrLn $ intercalate "\n" (map showBoard boards)
      putStrLn ""

      putStrLn "[Comparison]"
      -- putStrLn $ "Draw Seqs: " ++ show draw_seqs
      -- mapM_ printSeqsToBoard seqs_to_board


      putStrLn "[WINNER!]"
      putStrLn $ "Board:\n" ++ showBoard win_board
      putStrLn ""
      putStrLn $ "Num Sum:   " ++ show win_sum
      putStrLn $ "Last Draw: " ++ show last_draw
      putStrLn $ "Score:     " ++ show (win_sum * last_draw)

      putStrLn ""
      putStrLn "[LOSER!]"
      putStrLn $ "Board:\n" ++ showBoard lose_board
      putStrLn ""
      putStrLn $ "Num Sum:   " ++ show lose_sum
      putStrLn $ "Last Draw: " ++ show last_lose_draw
      putStrLn $ "Score:     " ++ show (lose_sum * last_lose_draw)

    


  where 
    boardScore :: [Int] -> Board -> Int
    boardScore draw_seq board = 
      let board_nums = concat board
       in sum (board_nums \\ draw_seq)
    
    boardWinSeq :: [[Int]] -> ([[Int]], Board) -> (Int, [Int], Board)
    boardWinSeq draw_seqs board_entry =
      let win_draw_seq = 
            find (`isWinningBoardEntry` board_entry) draw_seqs
       in case win_draw_seq of 
            Nothing -> error $ "Board doesn't have a win sequence: " ++ showBoard (snd board_entry)
            Just win_draw -> (length win_draw, win_draw, snd board_entry)

    fst3 :: (a,b,c) -> a
    fst3 (x,_,_) = x

    printSeqsToBoard :: ([[Int]], Board) -> IO ()
    printSeqsToBoard (sqs, b) =
      do putStrLn "[Seqs]"
         putStrLn $ showBoard sqs
         putStrLn "[Board]"
         putStrLn $ showBoard b

    isWinningBoardEntry :: [Int] -> ([[Int]], Board) -> Bool
    isWinningBoardEntry draw_seq (board_seqs, _) = any (`isSubsequenceOf` draw_seq) board_seqs


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