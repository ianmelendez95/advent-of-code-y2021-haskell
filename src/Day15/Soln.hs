{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day15.Soln where 

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR

import Data.Ord (comparing)
import Data.List
import Data.Maybe (maybe, fromMaybe, listToMaybe, catMaybes, mapMaybe)
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

import Debug.Trace


type Point = (Int, Int)
type PointMap a = Map Point a

type Path = (Point, Set Point)
type PointCosts  = Map Point Int
type PathCosts   = Map Point Int
type PointPriors = IntMap [Path]

type PointDeque = [Point]

queuePoints :: [Point] -> PointDeque -> PointDeque
queuePoints = flip (++)

inputFile = "src/Day15/full-input.txt"
inputDim = 100

soln :: IO ()
soln = 
  do point_costs <- parseInput <$> TIO.readFile inputFile
    --  printPointMap point_costs
    --  putStrLn $ show (Map.findMax point_costs)

     let path_costs = bfsPathCosts [(0,0)] point_costs (Map.singleton (0,0) 0)
         end_point = fst (Map.findMax point_costs)

    --  printPointMap path_costs
     print (Map.lookup end_point path_costs)
     

    --  let point_priors :: PointPriors
    --      point_priors = IntMap.singleton 0 [(fst (Map.findMin point_costs), Set.empty)]

        --  end_point = fst (Map.findMax point_costs)

        --  prior_iters = iterate (iterPriorQueue point_costs) point_priors
        --  idx_prior_iters = zip [0..] prior_iters 

        --  shortest_path = head $ mapMaybe (shortestInPriors end_point) prior_iters
        --  shortest_path_cost = fst shortest_path

    --  mapM_ printPriors (take 10 prior_iters)
    --  printPriors (idx_prior_iters !! 20)
    --  print shortest_path
    --  putStrLn $ "Shortest Cost: " ++ show shortest_path_cost

    --  let all_paths = findPaths (Map.keysSet point_map) (fst (Map.findMin point_map)) (fst (Map.findMax point_map))

    --  mapM_ print (take 5 all_paths)
    --  print (length all_paths)

  where 
    printPriors :: (Int, PointPriors) -> IO ()
    printPriors (iter, priors) = 
      do putStrLn $ "\n[[Gen " ++ show iter ++ "]]"
         mapM_ printPriorEntry . IntMap.toList $ priors

    printPriorEntry :: (Int, [Path]) -> IO ()
    printPriorEntry (cost, paths) = 
      do print cost
         mapM_ printPath paths
         
    printPath :: Path -> IO ()
    printPath (cur_point, points_tail) = 
      do putStr $ "  " ++ show cur_point ++ " - "
         putStrLn $ intercalate "," (map show (Set.toList points_tail))


bfsPathCosts :: [Point] -> PointCosts -> PathCosts -> PathCosts
bfsPathCosts [] _ path_cs = path_cs
bfsPathCosts (p:ps) base_point_cs path_cs = 
  let adj_point_path_updates = 
        mapMaybe adjacentPathCostUpdate (adjacentPoints p)
   in bfsPathCosts (ps ++ map fst adj_point_path_updates)
                   base_point_cs 
                   (foldl' (\cs (p, c) -> Map.insert p c cs) path_cs adj_point_path_updates)
  where 
    cur_path_cost :: Int
    cur_path_cost = 
      case path_cs Map.!? p of 
        Nothing -> error $ "No path cost for current point: " ++ show p
        Just c -> c
    
    mPointCost :: Point -> Maybe Int
    mPointCost point@(x,y) =
      if pointInBounds point
        then let base_point = (x `mod` inputDim, y `mod` inputDim)
                 tile_tax = x `div` inputDim + y `div` inputDim

                 raw_cost = tile_tax + (base_point_cs Map.! base_point)
              in if raw_cost <= 9
                   then Just raw_cost
                   else Just $ ((raw_cost - 10) `mod` 9) + 1
        else Nothing
    
    pointInBounds :: Point -> Bool
    pointInBounds (x,y) = x >= 0 && y >= 0 && x < 5 * inputDim && y < 5 * inputDim

    adjacentPathCostUpdate :: Point -> Maybe (Point, Int)
    adjacentPathCostUpdate adj_p = 
      case mPointCost adj_p of
        Nothing -> Nothing
        Just adj_point_cost -> 
          let cur_adj_path_cost = cur_path_cost + adj_point_cost
          in  case Map.lookup adj_p path_cs of 
                Nothing -> Just (adj_p, cur_adj_path_cost)
                Just existing_adj_path_cost -> 
                  if cur_adj_path_cost < existing_adj_path_cost 
                    then Just (adj_p, cur_adj_path_cost)
                    else Nothing


shortestInPriors :: Point -> PointPriors -> Maybe (Int, Path)
shortestInPriors end_point priors = 
  let (min_cost, min_paths) = IntMap.findMin priors
      min_end_path = find ((== end_point) . fst) min_paths
   in (min_cost,) <$> min_end_path

-- | resolve the next paths of the minimal path
iterPriorQueue :: PointCosts -> PointPriors -> PointPriors
iterPriorQueue costs priors =
  let (min_cost, min_paths) = IntMap.findMin priors

      priors' :: PointPriors
      priors' = IntMap.delete min_cost priors
      
      next_paths :: [(Int, Path)]
      next_paths = concatMap (adjPaths min_cost) min_paths

   in foldl' (\ps (c, p) -> priorsInsertPath c p ps) priors' next_paths
  where 
    adjPaths :: Int -> Path -> [(Int, Path)]
    adjPaths pc (p, ps) = 
      let ps' :: Set Point
          ps' = Set.insert p ps
        
          aps :: [Point]
          aps = filter (\ap -> not (ap `Set.member` ps) && (ap `Map.member` costs)) 
                       (adjacentPoints p)

          aps_apcs :: [(Int, Point)]
          aps_apcs = map (toFst ((+ pc) . (costs Map.!))) aps

       in map ((,ps') <$>) aps_apcs

priorsInsertPath :: Int -> Path -> PointPriors -> PointPriors
priorsInsertPath c p = IntMap.insertWith (++) c [p]
    
toFst :: (a -> b) -> a -> (b,a)
toFst f x = (f x, x)

toSnd :: (a -> b) -> a -> (a,b)
toSnd f x = (x, f x)

-- findPaths :: Set Point -> Point -> Point -> [Path]
-- findPaths all_points start_point end_point 
--   | start_point == end_point = [[end_point]]
--   | otherwise = 
--     let adj_points = filter (`Set.member` all_points) (adjacentPoints start_point)
--         del_start = Set.delete start_point all_points
--      in map (start_point :) $ concatMap (\ap -> findPaths del_start ap end_point) adj_points



adjacentPoints :: Point -> [Point]
adjacentPoints (r,c) = 
  [ (r - 1, c)
  , (r + 1, c)
  , (r,     c - 1)
  , (r,     c + 1)
  ]

printPointMap :: PointMap Int -> IO ()
printPointMap levels = printEntries (Map.toList levels)
  where 
    printEntries :: [(Point, Int)] -> IO ()
    printEntries [] = pure () 
    printEntries [(_,l)] = putStrLn (showCell l)
    printEntries (((m,n),l):rest@(((m',_),_):_))
      | m' > m = putStrLn (showCell l) >> printEntries rest
      | otherwise = putStr (showCell l) >> printEntries rest
    
    showCell :: Int -> String
    showCell n = 
      let n_str = show n
       in replicate (10 - length n_str) ' ' ++ n_str

parseInput :: T.Text -> PointMap Int
parseInput input = Map.fromList . concat $ zipWith lineToPoints [0..] (T.lines input)
  where
    lineToPoints :: Int -> T.Text -> [(Point, Int)]
    lineToPoints row_idx line = 
      zipWith (\col_idx height -> ((row_idx, col_idx), height)) [0..] (rowHeights line)

    rowHeights :: T.Text -> [Int]
    rowHeights line = map digitToInt (T.unpack line)
