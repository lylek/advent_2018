#!/usr/bin/env stack
-- stack script --resolver lts-12.2

import System.IO (readFile)
import qualified Data.IntSet as IS (empty, member, insert)
import Data.List (find)

main :: IO ()
main = do
    contents <- readFile "day_1_input.txt"
    let offsets = map readOffset $ lines contents
    let frequencies = scanl (+) 0 $ cycle offsets
    let allSeen = scanl
            (\(seenSoFar, _, wasSeen) n -> (IS.insert n seenSoFar, n, IS.member n seenSoFar))
            (IS.empty, 0, False)
            frequencies
    let firstSeen = find (\(_, _, wasSeen) -> wasSeen) allSeen
    putStrLn (case firstSeen of
        Nothing -> "no frequencies seen twice"
        Just (_, n, _) -> show n)

readOffset :: String -> Int
readOffset (sign : digits) =
    let signum = case sign of
            '+' -> 1
            '-' -> -1
    in signum * (read digits)
