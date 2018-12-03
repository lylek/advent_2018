#!/usr/bin/env stack
-- stack script --resolver lts-12.2

-- Day 2 Part 2

import Control.Monad (join)
import Data.List (find)
import Data.Maybe (isJust)
import qualified Data.Text.IO as TIO
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

main :: IO ()
main = do
    contents <- TIO.readFile "day_2_input.txt"
    let ids = T.lines contents
        width = T.length $ head ids
        mDups = flip map [0..(width-1)]
            (\i ->
                let idsMissingColumn = map (dropNthChar i) ids
                in findDuplicate idsMissingColumn)
        Just dup = join $ find isJust mDups
    TIO.putStrLn dup

dropNthChar :: Int -> Text -> Text
dropNthChar n s =
    let (l, r) = T.splitAt n s
    in T.append l (T.tail r)

findDuplicate :: Ord a => [a] -> Maybe a
findDuplicate xs =
    join
    $ find isJust
    $ map snd
    $ scanl
        (\(seen, _) x -> (Set.insert x seen, (if Set.member x seen then Just x else Nothing)))
        (Set.empty, Nothing)
        xs