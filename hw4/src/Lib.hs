{-# LANGUAGE BangPatterns #-}
module Lib
    ( Point
    , multiply
    , multiplyNaive
    , multiplyVectorParallel
    , randomMatrix
    , doubleArea
    , doubleAreaNaive
    , doubleAreaNaiveST
    , doubleAreaST
    , perimeter
    , perimeterNaive
    , perimeterNaiveST
    , perimeterST
    , randomListOfPoints
    ) where

import Data.List ( transpose, foldl', cycle )
import Control.Parallel.Strategies ( rpar, rseq, runEval, rdeepseq, parListChunk, parList, dot, using, parMap )
import Data.List.Split ( chunksOf )
import Control.Monad ( replicateM, forM_ )
import Control.Monad.ST ( runST )
import Control.DeepSeq ( NFData (rnf) )
import Data.STRef ( modifySTRef, modifySTRef', readSTRef, newSTRef, writeSTRef )
import System.Random ( newStdGen, randomRs )
import qualified Data.Vector as V

randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen

randomMatrix :: Int -> Int -> IO [[Int]]
randomMatrix n m = replicateM n (randomIntList m 0 100000)

randomListOfPoints :: Int -> IO [Point]
randomListOfPoints n = do
  x <- randomIntList n 0 100000
  y <- randomIntList n 0 100000
  return $ zipWith Point x y

--Task 1
multiply :: [[Int]] -> [[Int]] -> Maybe [[Int]]
multiply m1 m2 = do
  let m2t = transpose m2
  let pairs = [(x,y) | x <- m1, y <- m2t]
  if (length (head m1) /= length m2)
    then
      mempty
    else (do
      let elems = runEval $ parListChunk 1000 rdeepseq (map (\p -> foldl' (+) 0 $ zipWith (*) (fst p) (snd p)) pairs)
      return $ chunksOf (length m1) elems)

multiplyNaive :: [[Int]] -> [[Int]] -> Maybe [[Int]]
multiplyNaive m1 m2 = do
  let m2t = transpose m2
  let pairs = [(x,y) | x <- m1, y <- m2t]
  if (length (head m1) /= length m2)
    then
      mempty
    else (do
      let elems = map (\p -> foldl' (+) 0 $ zipWith (*) (fst p) (snd p)) pairs
      return $ chunksOf (length m1) elems)

multiplyVectorParallel :: [[Int]] -> [[Int]] -> Maybe [[Int]]
multiplyVectorParallel m1 m2 = do
  let m2t = transpose m2
  let v1 = V.fromList (map (\l -> V.fromList l) m1)
  let v2 = V.fromList (map (\l -> V.fromList l) m2t)
  let len = length (head m1)
  let pairs = [(v1 V.! i, v2 V.! j) | i <- [0..(length v1 - 1)], j <- [0..(length v2 - 1)]]
  if (len /= length m2)
    then
      mempty
    else (do
      let elems = runEval $ parListChunk 1000 rdeepseq $ map (\p -> foldl' (+) 0 $ V.zipWith (*) (fst p) (snd p)) pairs
      return $ chunksOf (length v1) elems)

--Task 2
data Point = Point Int Int deriving (Show, Eq)

instance NFData Point where
  rnf (Point x y) = x `seq` y `seq` ()

plus :: Point -> Point -> Point
plus (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

minus :: Point -> Point -> Point
minus (Point x1 y1) (Point x2 y2) = Point (x1 - x2) (y1 - y2)

scalarProduct :: Point -> Point -> Int
scalarProduct (Point x1 y1) (Point x2 y2) = let res = (x1 * x2) + (y1 * y2) in res `seq` res

crossProduct :: Point -> Point -> Int
crossProduct (Point x1 y1) (Point x2 y2) = let res = (x1 * y2) - (y1 * x2) in res `seq` res

distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = sqrt $! fromIntegral $ (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1)

perimeter :: [Point] -> Double
perimeter points = let h = (head points) in calc (tail points) h 0 h
  where
    calc [] prev !acc first = acc + distance prev first
    calc (x:xs) prev !acc first = let newR = acc + (distance prev x) in calc xs x newR first

perimeterNaive :: [Point] -> Double
perimeterNaive points = abs $ foldl' (+) 0 $ (map (\p -> distance (fst p) (snd p)) pairs)
  where
    shifted = (last points) : (init points)
    pairs = zip points shifted

perimeterNaiveST  :: [Point] -> Double
perimeterNaiveST points = abs $ sumST $ (map (\p -> distance (fst p) (snd p)) pairs)
  where
    shifted = (last points) : (init points)
    pairs = zip points shifted

perimeterST :: [Point] -> Double
perimeterST points = runST $ do
  n <- newSTRef 0
  lt <- newSTRef (head points)
  prev <- newSTRef (head points)
  forM_ (tail points) $ (\x -> do
    pr <- readSTRef prev
    modifySTRef n (+ distance pr x)
    writeSTRef prev x
    writeSTRef lt x)
  l <- readSTRef lt
  modifySTRef n (+ distance l (head points))
  readSTRef n

doubleArea :: [Point] -> Int
doubleArea points = let h = (head points) in abs $ calc (tail points) h 0 h
  where
    calc [] prev !acc first = acc + crossProduct prev first
    calc (x:xs) prev !acc first = let newR = acc + (crossProduct prev x) in calc xs x newR first

doubleAreaNaive :: [Point] -> Int
doubleAreaNaive points = abs $ foldl' (+) 0 $ (map (\p -> crossProduct (fst p) (snd p)) pairs)
  where
    shifted = (last points) : (init points)
    pairs = zip points shifted

doubleAreaNaiveST  :: [Point] -> Int
doubleAreaNaiveST points = abs $ sumST $ (map (\p -> crossProduct (fst p) (snd p)) pairs)
  where
    shifted = (last points) : (init points)
    pairs = zip points shifted

doubleAreaST :: [Point] -> Int
doubleAreaST points = runST $ do
  n <- newSTRef 0
  lt <- newSTRef (head points)
  prev <- newSTRef (head points)
  forM_ (tail points) $ (\x -> do
    pr <- readSTRef prev
    modifySTRef n (+ crossProduct pr x)
    writeSTRef prev x
    writeSTRef lt x)
  l <- readSTRef lt
  modifySTRef n (+ crossProduct l (head points))
  readSTRef n

sumST :: Num a => [a] -> a
sumST xs = runST $ do
  n <- newSTRef 0
  forM_ xs $ (\x -> do
    modifySTRef' n (+ x))
  readSTRef n

--Task 3
gauss :: [[Bool]] -> [Bool] -> Maybe [Bool]
gauss matrix results = do
  let triangle = gaussify matrix results
  mempty  -- неуспел(((99
  

removeZeroes :: V.Vector (V.Vector (Bool)) -> Maybe (V.Vector (V.Vector (Bool)))
removeZeroes matrix = let noNulls = V.filter (\row -> V.or row) matrix in
                        if (Lib.not $ V.null $ V.filter (\row -> Lib.not (V.or $ (V.init row)) `Lib.and` (V.last row)) noNulls)
                        then Nothing
                        else Just noNulls

gaussify :: [[Bool]] -> [Bool] -> V.Vector (V.Vector (Bool))
gaussify matrix results = V.foldl reduceRow zipped (V.fromList [0..length matrix - 1])
  where
    zipped = V.fromList (runEval $ parListChunk 100 rdeepseq $ map (\(l, res) -> (V.fromList l) `V.snoc` res) (zip matrix results))
    swap xs a b = if (a == b)
                  then xs
                  else xs V.// [(a, xs V.! b), (b, xs V.! a)]
    reduceRow matrix index = (V.take (index + 1) switched) V.++ (V.map nullifyRow (V.drop (index + 1) switched))
      where
        firstIndex = head $ filter (\x -> matrix V.! x V.! index) [index..length matrix - 1]
        switched = swap matrix index firstIndex
        nullifyRow = \row -> if (row V.! index)
                             then V.zipWith Lib.xor (switched V.! index) row
                             else row

verifySolution :: [[Bool]] -> [Bool] -> [Bool] -> Bool
verifySolution matrix results answers = foldl' Lib.and True matches
  where
    matches = zipWith Lib.xor answers (runEval $ parListChunk 1000 rdeepseq $ map (\l -> foldl' Lib.xor False $ zipWith Lib.and l answers) matrix)

not :: Bool -> Bool
not True = False
not False = True

and :: Bool -> Bool -> Bool
and True True = True
and _ _ = False

xor :: Bool -> Bool -> Bool
xor True True = False
xor False False = False
xor _ _ = True