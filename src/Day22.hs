module Day22 where

-- start 13h41 -> 13h58. pause

import Utils
import Path

import qualified Data.Text as Text

fileContent = Cave 3066 (13, 726)
example = Cave 510 (10, 10)

data Cave = Cave
  { depth :: Int
  , target :: (Int, Int)
  } deriving (Show)


deriveMemoizable ''Cave

-- * Generics
geoIndex _f _cave (0, 0) = 0
geoIndex _f cave i
  | target cave == i = 0
geoIndex _f _cave (x, 0) = x * 16807
geoIndex _f _cave (0, y) = y * 48271
geoIndex f cave (x, y) = f cave (x - 1, y) * f cave (x, y - 1)

erosionLevelFix f cave i = (geoIndex f cave i + depth cave) `mod` 20183

erosionLevel = memoFix2 erosionLevelFix

regionType cave coord = case erosionLevel cave coord `mod` 3 of
  0 -> Rocky
  1 -> Wet
  2 -> Narrow

data RegionType = Rocky | Wet | Narrow deriving (Show)

drawMap :: Cave -> (Int, Int) -> Text
drawMap cave (xMax, yMax) = Text.unlines $ do
  y <- [0..yMax]

  pure $ Text.pack $ do
    x <- [0..xMax]

    let coord = (x, y)
    pure $ case regionType cave coord of
      Rocky -> '.'
      Wet -> '='
      Narrow -> '|'

riskLevel :: Cave -> Int
riskLevel cave = let
  (xMax, yMax) = target cave

  in sum $ do
  y <- [0..yMax]
  x <- [0..xMax]

  let coord = (x, y)
  pure $ case regionType cave coord of
      Rocky -> 0
      Wet -> 1
      Narrow -> 2


-- * FIRST problem
day :: _ -> Int
day = riskLevel

-- * SECOND problem
data RescueTool = Nop | Torch | ClimbingGear
  deriving (Show, Generic, GEnum, Eq, Ord)
  
day' cave = fst <$> shortestPath (transition cave) (+) ((0, 0), Torch) (toTarget (target cave))
 

type Status = ((Int, Int), RescueTool)

toTarget coord = (coord, Torch)

transition :: Cave -> ((Int, Int), RescueTool) -> [(Int, ((Int, Int), RescueTool))]
transition cave current@(currentCoord, currentTool) = let
  ts = map (\t -> (7, (currentCoord,t))) (transitionSelf cave current)
  tc = map (\c -> (1, (c, currentTool))) (transitionClose cave current)
  in ts ++ tc
  

available Rocky Nop = False
available Wet Torch = False
available Narrow ClimbingGear = False
available _ _ = True

transitionSelf :: Cave -> ((Int, Int), RescueTool) -> [RescueTool]
transitionSelf cave (coord, currentT) = filter (\t -> available (regionType cave coord) t && t /= currentT) genum

transitionClose :: Cave -> ((Int, Int), RescueTool) -> [(Int, Int)]
transitionClose cave ((x, y), currentT) = do
  (dx, dy) <- [
    (-1, 0),
    (1, 0),
    (0, -1),
    (0, 1)]
  let nextCoord@(x', y') = (x + dx, y + dy)
  guard $ x' >= 0
  guard $ y' >= 0

  guard $ available (regionType cave nextCoord) currentT

  pure nextCoord

-- * Tests

test :: Spec
test = do
 describe "woks" $ do
   it "on first star" $ do
     day fileContent `shouldBe` 10115
   it "on second star" $ do
     day' fileContent `shouldBe` Just 990
