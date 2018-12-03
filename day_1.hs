#!/usr/bin/env stack
-- stack script --resolver lts-12.20

import System.IO (readFile)

main :: IO ()
main = do
    contents <- readFile "day_1_input.txt"
    let frequency = sum $ map readOffset $ lines contents
    print frequency

readOffset :: String -> Int
readOffset (sign : digits) =
    let signum = case sign of
            '+' -> 1
            '-' -> -1
    in signum * (read digits)

            
