#!/usr/bin/env stack
-- stack script --resolver lts-12.2

-- Day 2 Part 1

import System.IO (readFile)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map
import Data.List (foldl')

main :: IO ()
main = do
    contents <- readFile "day_2_input.txt"
    let ids = lines contents
        letterCountsPerId = map counts ids
        countsOfCountsPerId = map (counts . Map.elems) letterCountsPerId
        countOccursAtLeastOncePerId = map (Map.map $ const 1) countsOfCountsPerId
        globalCountOfCounts = foldl' (mergeMapsBy (+)) Map.empty countOccursAtLeastOncePerId
        appearingTwice = Map.findWithDefault 0 2 globalCountOfCounts
        appearingThrice = Map.findWithDefault 0 3 globalCountOfCounts
    print (appearingTwice * appearingThrice)

counts :: Ord a => [a] -> Map a Int
counts xs = foldl' (\counts x -> Map.alter incCount x counts) Map.empty xs

incCount :: Maybe Int -> Maybe Int
incCount Nothing = Just 1
incCount (Just n) = Just (n + 1)

-- Merges two maps by applying the provided binary function for combining the values
-- when the key occurs in both maps. Also includes values where the key occurs in
-- only one of the maps.
mergeMapsBy :: Ord k => (v -> v -> v) -> Map k v -> Map k v -> Map k v
mergeMapsBy f m1 m2 =
    Map.merge Map.preserveMissing Map.preserveMissing
        (Map.zipWithMatched (\k x y -> f x y)) m1 m2
