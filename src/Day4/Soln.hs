{-# LANGUAGE OverloadedStrings #-}

module Day4.Soln where 

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR


import Data.List (foldl', transpose)


inputFile :: FilePath
inputFile = "src/Day4/short-input.txt"


soln :: IO ()
soln = 
  do content <- TIO.readFile inputFile
     _