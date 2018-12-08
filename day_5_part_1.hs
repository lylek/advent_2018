#!/usr/bin/env stack
-- stack script --resolver lts-12.2

-- Day 5 Part 1

import Data.Char (toUpper)
import System.IO (readFile)

main :: IO ()
main = do
    contents <- readFile "day_5_input.txt"
    let initialPolymer = head $ lines contents
        reactedPolymer = react initialPolymer
    print $ length reactedPolymer

-- Cancel out adjacent pairs of letters that match except for case,
-- repeatedly until there are no more to cancel.
--
react :: String -> String
react polymer = reactZipper "" polymer

-- Helper function that performs the real work of react. At any point
-- it is focused on a particular point in the string to react. The
-- first argument is the left side, in reverse order, and the second
-- argument is the right side, in forwards order. Thus it implements
-- the zipper pattern.
--
reactZipper :: String -> String -> String
reactZipper left "" = left
reactZipper "" (x : right) = reactZipper [x] right
reactZipper (x : left) (y : right) =
    if x /= y && toUpper x == toUpper y
        then reactZipper left right
        else reactZipper (y : x : left) right
