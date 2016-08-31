module Main where

import System.IO
import Data.List.Split (splitOn)
import qualified Data.Time.Calendar as T (Day(..), fromGregorian)

type FileName = String
type CSVString = String

data Series = TimeSeries { tseries :: [Float] } | NumberSeries { nseries :: [Float] }
    deriving Show

data Bars = Bars {
    date :: Series,
    open :: Series,
    high :: Series,
    low :: Series,
    close :: Series,
    adjclose :: Series,
    volumes :: Series
} deriving Show

-- Name : readCsv
-- Input : FileName (String)
-- Output : IO CSVString
-- Uses : readFile
readCsv :: FileName -> IO CSVString
readCsv fileName = readFile fileName

-- Name : parseCsv
-- Input : CSVString (String)
-- Output : [[String]]
-- Uses : drop, lines, splitOn
parseCsv :: CSVString -> [[String]]
parseCsv csvString =
    let csvRows = drop 1 (lines csvString)
    in [ splitOn ";" csvRow | csvRow <- csvRows ]

-- Name : strToInt
-- Input : String
-- Output : Integer
-- Uses : read
stringToInteger :: String -> Integer
stringToInteger str = read str :: Integer


-- Name : makeDay
-- Input: String
-- Output: T.Day
-- Uses : splitOn, strToInt, T.fromGregorian
makeDay :: String -> T.Day
makeDay date =
    let dateData = splitOn "-" date
        year = stringToInteger (dateData !! 0)
        month = fromIntegral (stringToInteger (dateData !! 1))
        day = fromIntegral (stringToInteger (dateData !! 2))
    in T.fromGregorian year month day


-- main
main :: IO()
main = do
    hSetBuffering stdout NoBuffering
    csvString <- readCsv "./data/msft.csv"

    let csvData = parseCsv csvString
    print(csvData)

