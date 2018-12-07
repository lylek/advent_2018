#!/usr/bin/env stack
-- stack script --resolver lts-12.2
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

-- Day 4 Part 1

import           Data.Function (on)
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.List (foldl', maximumBy, sort)
import           Data.Vector (Vector)
import qualified Data.Vector as Vec
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TRead
import Text.Regex.PCRE.Heavy (re, scan)

data Record = Record
    { rDate :: !Text
    , rHour :: !Int
    , rMinute :: !Int
    , rAction :: !Action
    } deriving (Show, Eq, Ord)

data Action =
    BeginsShift !Int
    | FallsAsleep
    | WakesUp
    deriving (Show, Eq, Ord)

data SleepRange = SleepRange
    { srGuardId :: !Int
    , srSleepMinute :: !Int
    , srWakeMinute :: !Int
    } deriving (Show)

main = do
    contents <- TIO.readFile "day_4_input.txt"
    let records = sort $ map parseRecord $ T.lines contents
        sleepRanges = recordsToSleepRanges records
        sleepMinutesPerGuard = sleepRangesToSleepMinutesPerGuard sleepRanges
        (sleepiest, _) = maximumBy (compare `on` snd)
            $ IntMap.toList sleepMinutesPerGuard
        sleepiestRanges = filter (\sr -> srGuardId sr == sleepiest) sleepRanges
        sleptMinutes = sleepRangesToSleptMinutes sleepiestRanges
        (mostSleptMinute, _) = maximumBy (compare `on` snd)
            $ Vec.toList $ Vec.indexed sleptMinutes
    print (sleepiest * mostSleptMinute)

recordsToSleepRanges :: [Record] -> [SleepRange]
recordsToSleepRanges records =
    let (Record {..} : records') = records
        BeginsShift guardId = rAction
    in sleepWakeRecordsToSleepRanges guardId records'

sleepWakeRecordsToSleepRanges :: Int -> [Record] -> [SleepRange]
sleepWakeRecordsToSleepRanges guardId records =
    case records of
        (Record _ _ _ (BeginsShift _) : _) ->
            recordsToSleepRanges records
        (sleepRecord : wakeRecord : records') ->
            SleepRange guardId (rMinute sleepRecord) (rMinute wakeRecord)
                : sleepWakeRecordsToSleepRanges guardId records'
        [] -> []

sleepRangesToSleepMinutesPerGuard :: [SleepRange] -> IntMap Int
sleepRangesToSleepMinutesPerGuard ranges =
    foldl'
        (\counts SleepRange{..} ->
            IntMap.alter (add (srWakeMinute - srSleepMinute)) srGuardId counts)
        IntMap.empty
        ranges
    where add n Nothing = Just n
          add n (Just t) = Just (n + t)

sleepRangesToSleptMinutes :: [SleepRange] -> Vector Int
sleepRangesToSleptMinutes ranges =
    Vec.accum (+) (Vec.replicate 60 0) $
        concatMap
            (\SleepRange{..} -> zip [srSleepMinute..(srWakeMinute-1)] (repeat 1))
            ranges

-- assumes the text represents a decimal integer,
-- otherwise you get a runtime error
readDec :: Text -> Int
readDec t =
    let Right (i, _) = TRead.decimal t
    in i

parseRecord :: Text -> Record
parseRecord line =
    let dateRE = [re|\[([0-9-]+) (\d\d):(\d\d)\] (.*)|]
        ((_, groups) : _) = scan dateRE line
        [rDate, tHour, tMinute, tAction] = groups
        rHour = readDec tHour
        rMinute = readDec tMinute
        beginsShiftRE = [re|Guard #(\d+) begins shift|]
        rAction = case scan beginsShiftRE tAction of
            ((_, [tGuardId]) : _) -> BeginsShift $ readDec tGuardId
            [] ->
                if tAction == "falls asleep"
                then FallsAsleep
                else WakesUp
    in Record{..}
