module Main where

import System.IO
import Data.List (elemIndex, sort, transpose)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import qualified Data.Time.Calendar as T (Day(..), fromGregorian)
import Debug.Trace
import Text.PrettyPrint.Boxes

type FileName = String
type CSVString = String
type WindowSize = Int
type BinSize = Int

data Series = TimeSeries { tseries :: [T.Day] }
    | NumberSeries { nseries :: [Float] }
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

-- Name : calculateDelta
-- Input : (Float, Float)
-- Output : Float
-- Uses :
calculateDelta :: (Float, Float) -> Float
calculateDelta fluple = (snd fluple - fst fluple) / fst fluple

-- Name : deltafy
-- Input : Series
-- Output : Series
-- Uses : calculateDelta, map, nseries, zip
deltafy :: Series -> Series
deltafy series =
    let unzippedSeries = nseries series
        zippedSeries = zip (0:unzippedSeries) unzippedSeries
        deltas = map calculateDelta zippedSeries
    in NumberSeries { nseries = deltas }

-- Name : makeWindow
-- Input : Int -> WindowSize -> Series
-- Output : [Float]
-- Uses : drop, start, take
makeWindow :: Int -> WindowSize -> Series -> [Float]
makeWindow start ws (NumberSeries series) =
    let withoutStart = drop start series
    in take ws withoutStart

-- Name : makeBin
-- Input : BinSize -> [Float]
-- Output : [Float]
-- Uses : fromIntegral, maximum, minimum
makeBin :: BinSize -> [Float] -> [Float]
makeBin binSize window =
    let min = minimum window
        max = maximum window
        step = (max - min) / fromIntegral (binSize - 1)
    in [ min + (step * fromIntegral x) | x <- [ 0 .. binSize - 1 ]]

-- Name : calculateBins
-- Input : Series -> WindowSize -> BinSize
-- Output : [[Float]]
-- Uses : length
calculateBins :: Series -> WindowSize -> BinSize -> [[Float]]
calculateBins (NumberSeries deltas) windowSize binSize =
    let range = [ 1 .. (length deltas) - windowSize]
    in [ makeBin binSize (makeWindow index windowSize (NumberSeries deltas)) | index <- range ]

-- Name : calculatePosterior
-- Input : Float -> Float -> Float
-- Output: Float
-- Uses : abs
calculatePosterior :: Float -> Float -> Float -> Float
calculatePosterior delta bin prior = (1 - abs (delta - bin)) * prior

-- Name : calculatePosteriors
-- Input : Float -> [Float] -> [Float]
-- Output : [Float]
-- Uses :
calculatePosteriors :: Float -> [Float] -> [Float] -> [Float]
calculatePosteriors delta bins priors =
    let binsAndPriors = zip bins priors
        posteriors = [ calculatePosterior delta bin prior | (bin, prior) <- binsAndPriors ]
        posteriorSum = sum posteriors
    in [ p / posteriorSum | p <- posteriors ]

calculateAllPosteriors :: Series -> [[Float]] -> [Float] -> [[Float]]
calculateAllPosteriors (NumberSeries []) _ _ = [[]]
calculateAllPosteriors _ [] _ = [[]]
calculateAllPosteriors (NumberSeries (delta:deltas)) (bin:bins) probs =
    let posteriors = calculatePosteriors delta bin probs
    in [posteriors] ++ calculateAllPosteriors (NumberSeries deltas) bins posteriors

-- Name : whichMax
-- Input : [Float]
-- Output : Int
-- Uses : elemIndex, fromJust, reverse, sort
whichMax :: [Float] -> Int
whichMax probabilities =
    let maximumProbability = (reverse $ sort probabilities) !! 0
    in fromJust (elemIndex maximumProbability probabilities)

-- Name : bestBins
-- Input : [[Float]] -> [[Float]]
-- Output : [Float]
-- Uses : bestBins, whichMax
bestBins :: [[Float]] -> [[Float]] -> [Float]
bestBins [] _ = []
bestBins _ [] = []
bestBins bins probabilities =
    let index = whichMax p
        bestBin = b !! index
    in [bestBin] ++ bestBins bs ps
    where (b:bs) = bins
          (p:ps) = probabilities

-- Name : calculatePrediction
-- Input : Float -> Float
-- Output : Float
-- Uses :
calculatePrediction :: Float -> Float -> Float
calculatePrediction serie bestBin = serie + bestBin * serie

-- Name : calculatePredictions
-- Input : Series -> WindowSize -> [Float]
-- Output : Series
-- Uses : calculatePrediction, drop, take, zip
calculatePredictions :: Series -> WindowSize -> [Float] -> Series
calculatePredictions (NumberSeries series) ws bestBins =
    let seriesBeforeWindow = take (ws + 1) series
        seriesAfterWindow = drop (ws + 1) series
        zippedSeries = zip seriesAfterWindow bestBins
        predictions = [ calculatePrediction serie bin | (serie, bin) <- zippedSeries ]
    in NumberSeries (seriesBeforeWindow ++ predictions)

-- Name : computeMape
-- Input : Series -> Series
-- Output : Float
-- Uses : abs, fromIntegral, sum, zip
computeMAPE :: Series -> Series -> Float
computeMAPE (NumberSeries original) (NumberSeries predicted) =
    let calcs = [ (abs (x - y)) / x | (x, y) <- zip original predicted ]
        originalAmount = fromIntegral (length original) :: Float
        meanAbsoluteError = (sum calcs) / originalAmount
    in meanAbsoluteError * 100

print_table :: [[String]] -> IO ()
print_table rows = printBox $ hsep 2 left (map (vcat left . map text) (transpose rows))

-- main
main :: IO()
main = do
    hSetBuffering stdout NoBuffering
    csvString <- readCsv "./data/msft.csv"

    let bars = makeBars csvString
    print ("bars", bars)
    putStrLn "|-----|"

    let deltas = deltafy (adjclose bars)
    print ("bars adjclose deltas", deltas)
    putStrLn "|-----|"

    let bins = calculateBins deltas 10 10
    print_table $ map (map show) bins
    putStrLn "|-----|"

    let priors = replicate 10 0.1
    let posteriors = calculateAllPosteriors (NumberSeries $ drop 10 $ nseries deltas) bins priors
    print_table $ map (map show) posteriors
    putStrLn "|-----|"

    let bb = bestBins bins posteriors
    print ("best bins", bb)
    putStrLn "|-----|"

    let predictions = calculatePredictions (adjclose bars) 10 bb
    print ("predictions", predictions)
    putStrLn "|-----|"

    let mape = computeMAPE (adjclose bars) predictions
    print ("mape", mape)
    putStrLn "|-----|"