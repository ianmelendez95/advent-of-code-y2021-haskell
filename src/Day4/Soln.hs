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
inputFile = "src/Day4/short-input.txt"


{-
[Part 1]

For part 1 we leverage `isSubsequenceOf` 
(a better alternative I didn't happen upon until later 
would be to leverage Set semantics, using `isSubsetOf`, 
detailed in [Set Approach]).

The idea is that each board has 10 winning 'sequences' 
of numbers. Each row is a winning sequence, and each column
is a winning sequence, so 5 rows + 5 columns = 10 sequences.

We sort each of these sequences to be able to perform an
efficient (O(n)) `isSubsequenceOf` on the list containing 
all drawn numbers so far, for each drawn number. 

Thus to setup we first collect

  1. Each cumulative sequence of draws
     e.g. if the draws are 
       
     7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

     then our 
     cumulative, sorted sequences would be 

     [[4,5,7,9,11],[4,5,7,9,11,17],[4,5,7,9,11,17,23],...]

  2. Each list of winning sequences of each board
     e.g. if we have the board 

     22 13 17 11  0
      8  2 23  4 24
     21  9 14 16  7
      6 10  3 18  5
      1 12 20 15 19

    Then its winning, sorted sequences are

    [0,11,13,17,22]
    [2,4,8,23,24]
    [7,9,14,16,21]
    [3,5,6,10,18]
    [1,12,15,19,20]
    [1,6,8,21,22]
    [2,9,10,12,13]
    [3,14,17,20,23]
    [4,11,15,16,18]
    [0,5,7,19,24]
    
With these in hand, we can then quickly identify (O(m*n), m = rows, n = cols)
whether any given board has won with any given draw sequence,
by observing if any of the winning sequences of the board 
'is a subsequence of' the sorted draw sequence.

Thus, we iterate through the cumulative sequences 
(simulating 'drawing' each number), and identify the 
first set of draws that results in a winning board,
and derive the score from the winning board and the 
last drawn number.


[Part 2]

Part 2 is very similar to Part 1. 
The difference is that for part 2 you want to identify
the *last* draw sequence to win for every board, 
and identify which board needed the most draws in
order to win (as opposed to part 1, where you
observed which board needed the *fewest* draws in 
order to win).


[Set Approach]

A better approach with better algorithmic complexity
would be to, instead of observing every 'sequence',
you observe every 'set'.

Then instead of observing whether any given 
winning sequence of numbers for a board 
is a subsequence of the drawn number sequence so far, 
you observe whether any given *winning set* of 
numbers is a *subset* of the *drawn number
set* so far.

This would result in operations 
of complexity O(logn) rather than O(n),
likely having a reduction in algorithmic
complexity of the whole process.
-}
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
      putStrLn $ "Draw Seqs: " ++ show draw_seqs
      mapM_ printSeqsToBoard seqs_to_board


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