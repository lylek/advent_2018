#!/usr/bin/env stack
-- stack script --resolver lts-12.2

-- Day 6 Part 2

import System.IO (readFile)

-- We now need to extend the region in which we search. By how much do we need
-- to extend it on each side? Extending it by the desired Manhattan distance
-- sum (10,000) divided by the number of coordinates (50), rounding down, will
-- do. (Thus, 200 in this case.)

-- Time:
--     Running as script, 17 seconds.
--     Compiled without optimization, 1.7 seconds.
--     Compiled with optimization, takes a bit over 0.3 seconds.

main :: IO ()
main = do
    contents <- readFile "day_6_input.txt"
    let allLines = lines contents
        allCoords = map readCoords allLines
    print $ areaWithinManhattanSum 10000 allCoords

areaWithinManhattanSum :: Int -> [(Int, Int)] -> Int
areaWithinManhattanSum maxDistSum allCoords =
    let searchRegion = searchRegionOf maxDistSum allCoords
        area = length $ filter (< maxDistSum) $ map (manhattanSum allCoords) searchRegion
    in area

readCoords :: String -> (Int, Int)
readCoords line =
    let (coordText1, (_ : _ : coordText2)) = span (/= ',') line
    in (read coordText1, read coordText2)

manhattanDist :: (Int, Int) -> (Int, Int) -> Int
manhattanDist (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

manhattanSum :: [(Int, Int)] -> (Int, Int) -> Int
manhattanSum coords point =
    sum $ map (manhattanDist point) coords

searchRegionOf :: Int -> [(Int, Int)] -> [(Int, Int)]
searchRegionOf maxDistSum allCoords =
    let xs = map fst allCoords
        ys = map snd allCoords
        minX = minimum xs
        maxX = maximum xs
        minY = minimum ys
        maxY = maximum ys
        nCoords = length allCoords
        marginWidth = div maxDistSum nCoords
    in [ (x, y)
       | x <- [minX - marginWidth .. maxX + marginWidth]
       , y <- [minY - marginWidth .. maxY + marginWidth]
       ]
