#!/usr/bin/env stack
-- stack script --resolver lts-12.2

-- Day 5 Part 2

import Data.Char (toUpper)
import Data.Function (on)
import Data.List (minimumBy, nub)
import System.IO (readFile)

main :: IO ()
main = do
    contents <- readFile "day_5_input.txt"
    let initialPolymer = head $ lines contents
        unitTypes = uniqueUnitTypes initialPolymer
        reactedLengthPerRemovedUnitType = map
            (\ut -> (ut, length $ react $ removeUnitType ut initialPolymer))
            unitTypes
        (_, bestLength) =
            minimumBy (compare `on` snd) reactedLengthPerRemovedUnitType
    print bestLength

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

uniqueUnitTypes :: String -> String
uniqueUnitTypes polymer = nub $ map toUpper polymer

removeUnitType :: Char -> String -> String
removeUnitType unitType polymer = filter (\x -> toUpper x /= unitType) polymer