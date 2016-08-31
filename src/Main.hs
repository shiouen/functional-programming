module Main where

import System.IO
import Data.List (sort, transpose)
import Data.List.Split (splitOn)
import qualified Data.Time.Calendar as T (Day(..), fromGregorian)

type FileName = String
type CSVString = String

data Series = TimeSeries { tseries :: [T.Day] } | NumberSeries { nseries :: [Float] }
    deriving Show

data Bars = Bars {
    date :: Series,
    open :: Series,
    high :: Series,
    low :: Series,
    close :: Series,
    adjclose :: Series,
    volume :: Series
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

-- Name : stringToFloat
-- Input : String
-- Output : Float
-- Uses : read
stringToFloat :: String -> Float
stringToFloat string = read string :: Float

makeSeries :: Char -> [String] -> Series
makeSeries option series
    | option == 'N' = NumberSeries { nseries = map stringToFloat series }
    | option == 'T' = TimeSeries { tseries = map makeDay series }

-- Name : makeBars
-- Input : CSVString
-- Output : Bars
-- Uses : map, reverse, parseCsv, transpose
makeBars :: CSVString -> Bars
makeBars csvString =
    let csvData = parseCsv csvString
        transposedCsvData = map reverse (transpose csvData)
        dates = makeSeries 'T' (transposedCsvData !! 0)
        opens = makeSeries 'N' (transposedCsvData !! 1)
        highs = makeSeries 'N' (transposedCsvData !! 2)
        lows = makeSeries 'N' (transposedCsvData !! 3)
        closes = makeSeries 'N' (transposedCsvData !! 4)
        adjcloses = makeSeries 'N' (transposedCsvData !! 6)
        volumes = makeSeries 'N' (transposedCsvData !! 5)
    in Bars {
        date = dates,
        open = opens,
        high = highs,
        low = lows,
        close = closes,
        adjclose = adjcloses,
        volume = volumes
    }

-- main
main :: IO()
main = do
    hSetBuffering stdout NoBuffering
    csvString <- readCsv "./data/msft.csv"

    let bars = makeBars csvString
    print(bars)

