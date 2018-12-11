#!/usr/bin/env stack
-- stack script --resolver lts-12.2

-- Day 6 Part 1

import           Control.Monad (join)
import           Data.List (foldl', span)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes)
import           System.IO (readFile)

-- This solution iterates over the bounding region of the coordinates, finding
-- the unique closest coordinate to each point. It takes advantage of the fact
-- that any coordinate which is the unique closest coordinate to some boundary
-- point will necessarily have an infinite area. Thus the closest coordinates
-- for the boundary and the interior can be calculated separately, and the
-- coords that have area on the boundary can be discarded from the map of
-- areas from the interior.
--
-- It might be faster to use an array and form concentric expanding rings
-- around each coordinate, until the whole region is filled.
--
-- But as is, it takes less than 0.1 seconds to find the solution, if compiled
-- with full optimization.

main :: IO ()
main = do
    contents <- readFile "day_6_input.txt"
    let allLines = lines contents
        allCoords = map readCoords allLines
    print $ maximumFiniteArea allCoords

maximumFiniteArea :: [(Int, Int)] -> Int
maximumFiniteArea allCoords =
    let xs = map fst allCoords
        ys = map snd allCoords
        minX = minimum xs
        maxX = maximum xs
        minY = minimum ys
        maxY = maximum ys
        boundary = boundaryOf (minX, minY) (maxX, maxY)
        interior = interiorOf (minX, minY) (maxX, maxY)
        boundaryAreas = countFreqs $ catMaybes $ map (closestCoord allCoords) boundary
        interiorAreas = countFreqs $ catMaybes $ map (closestCoord allCoords) interior
        boundaryKeys = Map.keys boundaryAreas
        finiteAreas = Map.difference interiorAreas boundaryAreas
    in maximum $ Map.elems finiteAreas

readCoords :: String -> (Int, Int)
readCoords line =
    let (coordText1, (_ : _ : coordText2)) = span (/= ',') line
    in (read coordText1, read coordText2)

manhattanDist :: (Int, Int) -> (Int, Int) -> Int
manhattanDist (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

closestCoord :: [(Int, Int)] -> (Int, Int) -> Maybe (Int, Int)
closestCoord coords point =
    fmap fst
    $ uniqueMinimumBy snd
    $ map (\coord -> (coord, manhattanDist coord point)) coords

-- This is linear (and tail recursive), whereas a sort would be O(lg n)
uniqueMinimumBy :: Ord b => (a -> b) -> [a] -> Maybe a
uniqueMinimumBy f xs = join $ fmap snd $ foldl' umb Nothing xs
    where
        umb Nothing x = Just (f x, Just x)
        umb acc@(Just (minVal, mbMinObj)) x =
            let fx = f x in
                case compare fx minVal of
                    LT -> (Just (fx, Just x))
                    EQ -> (Just (fx, Nothing))
                    GT -> acc

-- Given the upper left corner and lower right corner, computes
-- a rectangle of coordinates forming the boundary of the space.
--
boundaryOf :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
boundaryOf (minX, minY) (maxX, maxY) =
    [(x, minY) | x <- [minX..maxX-1]]
    ++ [(maxX, y) | y <- [minY..maxY-1]]
    ++ [(x, maxY) | x <- [minX+1..maxX]]
    ++ [(minX, y) | y <- [minY+1..maxY]]

-- Given the upper left corner and lower right corner, computes
-- the interior of a rectangle of coordinates forming the boundary
-- of the space.
--
interiorOf :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
interiorOf (minX, minY) (maxX, maxY) =
    [(x, y) | x <- [minX+1..maxX-1], y <- [minY+1..maxY-1]]

countFreqs :: Ord a => [a] -> Map a Int
countFreqs xs = foldl' (\m x -> Map.alter inc x m) Map.empty xs
    where
        inc Nothing = Just 1
        inc (Just n) = Just (n + 1)
