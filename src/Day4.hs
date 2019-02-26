module Day4 where

import Utils

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import qualified Data.Map as Map

import Data.Time.Clock
import Data.Time (fromGregorian)

data Behavior = WakesUp
              | FallsAsleep
              | BeginShift Int
              deriving (Show)

data Event = Event UTCTime Behavior
              deriving (Show)

data TimeRange = TimeRange UTCTime UTCTime
  deriving (Show)

-- start at 15h18
-- pause at 15h34
-- start again at 15h37
-- stop again at 15h56

fileContent :: Text
fileContent = $(getFile)

content = sortBy (comparing (\(Event t _) -> t)) $ unsafeParse (parseEvent `sepBy` "\n") fileContent

parseTime :: Parser UTCTime
parseTime = do
  y <- decimal
  "-"
  m <- decimal
  "-"
  d <- decimal
  " "
  h <- decimal
  ":"
  min <- decimal

  let day = fromGregorian y m d
  let time = secondsToDiffTime (fromIntegral (min * 60 + h * 360))

  pure (UTCTime day time)

parseBehavior = choice [
  "wakes up" $> WakesUp,
  "falls asleep" $> FallsAsleep,
  ("Guard #" *> (BeginShift <$> decimal) <* " begins shift")
  ]

-- * Generics
parseEvent = do
  "["
  t <- parseTime
  "] "
  behavior <- parseBehavior
  pure (Event t behavior)

eventToGuardMap :: [Event] -> Map Int [TimeRange]
eventToGuardMap es = go es (Map.empty) Nothing Nothing
  where
    go :: [Event] -> Map Int [TimeRange] -> Maybe UTCTime -> Maybe Int -> Map Int [TimeRange]
    go [] m _ _ = m
    go ((Event t event):xs) m currentStatus currentGuard = case event of
      FallsAsleep -> go xs m (Just t) currentGuard
      WakesUp -> let Just g = currentGuard
                     Just tStart = currentStatus
                 in
                   go xs (Map.insertWith (++) g [TimeRange tStart t] m) Nothing currentGuard
      BeginShift i -> go xs m Nothing (Just i)
        
guardSleep :: [TimeRange] -> NominalDiffTime
guardSleep tr = sum (map trTime tr)
  where
    trTime (TimeRange a b) = diffUTCTime b a

pickBestGuard :: Map Int [TimeRange] -> (Int, [TimeRange])
pickBestGuard m = maximumBy (comparing (guardSleep . snd)) (Map.toList m)

pickBestSecond :: [TimeRange] -> Integer
pickBestSecond tr = let
  ranges = concatMap timeRangeToRange tr
  m = Map.fromListWith (+) (map (,1) ranges)
  in fst (maximumBy (comparing snd) (Map.toList m))

pickBestSecond' :: [TimeRange] -> (Integer, Integer)
pickBestSecond' tr = let
  ranges = concatMap timeRangeToRange tr
  m = Map.fromListWith (+) (map (,1) ranges)
  in maximumBy (comparing snd) (Map.toList m)

timeRangeToRange :: TimeRange -> [Integer]
timeRangeToRange (TimeRange start end) =
  let tStart = (diffTimeToPicoseconds $ utctDayTime start) `div` (10 ^ 12) `div` 60
      tEnd = (diffTimeToPicoseconds $ utctDayTime end) `div` (10 ^ 12) `div` 60

    in [tStart .. (tEnd -1)]

-- * FIRST problem
day :: _ -> Int
day e = let
  (guard, tr) = pickBestGuard $ eventToGuardMap e
  sec = pickBestSecond tr
  in (guard * fromIntegral sec)

-- first start: 20h39
-- second start: 20h47

-- * SECOND problem
day' :: [Event] -> _
day' e = let
  guardMap = eventToGuardMap e
  guardBestSecond = Map.toList $ map pickBestSecond' guardMap
  (guard, (bestSec, count)) = maximumBy (comparing (snd . snd)) guardBestSecond
  in guard * fromIntegral bestSec
  

-- * Tests

test :: Spec
test = do
--   describe "simple examples" $ do
--     it "of first star" $ do
--       day "" `shouldBe` 0
--     it "of second star" $ do
--       day' "" `shouldBe` 0
  describe "woks" $ do
    it "on first star" $ do
      day content `shouldBe` 143415
    it "on second star" $ do
      day' content `shouldBe` 49944
