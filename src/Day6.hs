module Day6 where

import Utils

import Text.Megaparsec
import Text.Megaparsec.Char.Lexer

import qualified Data.Text as Text
import Data.Char (toLower)

import Control.Monad (guard)

import qualified Data.Map as Map
import qualified Data.Set as Set

-- start 21h25
-- pause 21h35: not motivated
-- restart 07h46 with more motivation, crappy internet connection (7h50 and I cannot load the webpage)

fileContent :: Text
fileContent = $(getFile)

content = unsafeParse (parseTuple `sepBy` "\n") fileContent

parseTuple :: Parser (Int, Int)
parseTuple = (,) <$> decimal <*> (", " *> decimal)

-- * Generics
manDistance (x, y) (x', y') = abs (x - x') + abs (y - y')

boundaries coords = (
  (minimum (map fst coords), minimum (map snd coords)), 
  (maximum (map fst coords), maximum (map snd coords)))

allCoords coords = let
  ((minX, minY), (maxX, maxY)) = boundaries coords
  in do
    x <- [minX .. maxX]
    y <- [minY .. maxY]
    pure (x, y)

data CoordStatus = Point (Int, Int) |
                   CoordStatus (Maybe (Int, Int)) Int
  deriving (Show)

regionGrowing coords = let
  ((minX, minY), (maxX, maxY)) = boundaries coords
  startMap = Map.fromList (map (\c -> (c,Point c)) coords)
  endMap = foldl' regionGrow startMap coords

  regionGrow :: Map (Int, Int) CoordStatus -> (Int, Int) -> Map (Int, Int) CoordStatus
  regionGrow m startingCoord = foldl' go m (friends startingCoord)
    where go curMap coord@(x, y)
            | x < minX || x > maxX || y < minY || y > maxY = curMap
            | otherwise = let
                nextMap = Map.insert coord (CoordStatus (Just startingCoord) (manDistance coord startingCoord)) curMap
              in case Map.lookup coord curMap of
              Just (Point _) -> curMap
              Nothing -> foldl' go nextMap (friends coord)
              Just (CoordStatus curCord d)
                | manDistance coord startingCoord < d -> foldl' go nextMap (friends coord)
                | manDistance coord startingCoord == d -> case curCord of
                    (Just oC) | oC /= startingCoord -> foldl' go (Map.insert coord (CoordStatus Nothing (manDistance coord startingCoord)) curMap) (friends coord)
                    _ -> curMap
                | otherwise -> curMap
  
  in endMap

friends (x, y) = do
  dx <- [-1 .. 1]
  dy <- [-1 .. 1]
  guard $ (dx, dy) /= (0, 0)
  pure (x + dx, y + dy)

onBoundaries coords regs =
  let
    ((minX, minY), (maxX, maxY)) = boundaries coords
    toto = Map.toList regs
    onBounds = do
      ((csX, csY), cs) <- toto
      guard $ csX == minX || csX == maxX || csY == minY || csY == maxY

      case cs of
        Point c -> [c]
        CoordStatus chien _ -> case chien of
          Nothing -> []
          Just c -> [c]
  in Set.fromList $ onBounds

regionSizes :: Map k CoordStatus -> Map (Int, Int) Int
regionSizes regs = let
  regions = do
    sts <- Map.elems regs
    case sts of
      Point d -> [d]
      CoordStatus p _ -> case p of
        Nothing -> []
        Just d -> [d]
  in Map.fromListWith (+) (map (,1) regions)

display coords regs = let
  coordDisp = Map.fromList (zip coords ['A'..])
  ((minX, minY), (maxX, maxY)) = boundaries coords
  in Text.unlines $ do
    y <- [minY .. maxY]
    pure $ Text.pack $ do
      x <- [minX .. maxX]
      let coord = (x, y)
      pure $ case Map.lookup coord regs of
        Nothing -> '?'
        Just (Point _) -> fromMaybe '?' (Map.lookup coord coordDisp)
        Just (CoordStatus Nothing _) -> '.'
        Just (CoordStatus (Just c) _) -> maybe '+' toLower (Map.lookup c coordDisp)


-- * FIRST problem
day :: [(Int, Int)] -> Int
day e = let
  regions = regionGrowing e
  sizes = regionSizes regions
  onB = onBoundaries e regions
  in maximum (map snd $ Map.toList $ Map.withoutKeys sizes onB)

-- * SECOND problem
day' e size = let
  ((minX, minY), (maxX, maxY)) = boundaries e
  e' = Set.fromList e
  toto = do
    x <- [minX .. maxX]
    y <- [minY .. maxY]

    let c = (x, y)

    -- guard $ Set.notMember c e'

    let d = sum (map (manDistance c) e)

    guard $ d < size
    pure c
  in length toto

  

-- * Tests
example = unsafeParse (parseTuple `sepBy` "\n") [here|1, 1
1, 6
8, 3
3, 4
5, 5
8, 9|]


-- test :: Spec
-- test = do
--   describe "simple examples" $ do
--     it "of first star" $ do
--       day "" `shouldBe` 0
--     it "of second star" $ do
--       day' "" `shouldBe` 0
--  describe "woks" $ do
--    it "on first star" $ do
--      day fileContent `shouldBe` 1228
--    it "on second star" $ do
--      day' fileContent `shouldBe` 1238

-- done at 23h25. Don't rememeber when restarted (something like 22h30)
-- star 2 23h37