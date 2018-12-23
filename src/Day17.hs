module Day17 where

import Utils

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text as Text

import Codec.Picture

-- start 23h18

fileContent = Map.fromSet (const Clay) $ unsafeParse parseContent $(getFile)

-- * Generics
parseRange = do
  start <- decimal
  end <- ".." *> decimal

  pure [start..end]

parseLine :: Parser (Set (Int, Int))
parseLine = Set.fromList <$> do
  firstCoord <- anyChar
  "="
  v <- decimal
  ", "
  anyChar
  "="
  range <- parseRange

  pure $ case firstCoord of
    'x' -> do
      let x = v
      y <- range

      pure (x, y)
    'y' -> do
      let y = v
      x <- range

      pure (x, y)
  

parseContent = mconcat <$> (parseLine `sepBy` "\n")

sizes s =
  let
    coords = map fst $ (filter (\(c, v) -> v == Clay) $ Map.toList s)
    minY = minimum (map snd coords)
    maxY = maximum (map snd coords)

    minX = minimum (map fst (Map.keys s))
    maxX = maximum (map fst (Map.keys s))
  in ((minX, maxX), (minY, maxY))

toImg s = 
  let
    ((minX, maxX), (minY, maxY)) = sizes s

    pixF x' y' = let
      x = x' + minX
      y = y' + minY
      in case Map.lookup (x, y) s of
         Just v -> case v of
           Clay -> PixelRGB8 255 0 0
           Water -> PixelRGB8 0 255 0
           Flow -> PixelRGB8 0 0 255
         Nothing -> PixelRGB8 255 255 255
  in generateImage pixF (maxX - minX + 1) (maxY - minY + 1)

display s =
  let
    coords = map fst $ (filter (\(c, v) -> v == Clay) $ Map.toList s)
    minY = minimum (map snd coords)
    maxY = maximum (map snd coords)

    minX = minimum (map fst (Map.keys s))
    maxX = maximum (map fst (Map.keys s))
  in Text.unlines $ do
    y <- [minY..maxY]
    pure $ Text.pack $ do
      x <- [minX..maxX]
      let c = (x, y)
      pure $ case Map.lookup c s of
        Nothing -> '.'
        Just Clay -> '#'
        Just Water -> '~'
        Just Flow -> '|'

data Case = Flow | Water | Clay
  deriving (Show, Eq, Ord)

flow :: Map (Int, Int) Case -> Map (Int, Int) Case
flow fieldStart = let (field', _) = goDown (500, 0) fieldStart
  in field'
  where
    maxY = maximum (map snd (Map.keys fieldStart))

    goDown coord@(x, y) field
       | y > maxY = (field, False)
       | otherwise = case Map.lookup coord field of
      Just Flow -> (field, False)
      Just Clay -> (field, True)
      Just Water -> scan coord (Map.insert coord Flow field)
      Nothing -> -- we can go down and depending on the result, we'll scan
        let (field', filled) = goDown (x, y+1) (Map.insert coord Flow field)
        in if not filled
           then (field', False)
           else scan coord (Map.insert coord Flow field')

    scanDx :: Int -> (Int, Int) -> Map (Int, Int) Case -> (Map (Int, Int) Case, Maybe Int)
    scanDx dx (x, y) field = let
      newCoord = (x + dx, y)
      continue = let (field', blockedDown) = goDown (x + dx, y + 1) (Map.insert newCoord Flow field)
                  in if blockedDown
                     then scanDx dx newCoord field'
                     else (field', Nothing)
      in case Map.lookup newCoord field of
         Just Clay -> (field, Just x)
         Just Flow -> (field, Nothing)
         Just Water -> (field, Just x)
         _ -> continue
                
    scan :: (Int, Int) -> Map (Int, Int) Case -> (Map (Int, Int) Case, Bool)
    scan coord@(_, y) field = let
      (field', blockedLeft) = scanDx (-1) coord field
      (field'', blockedRight) = scanDx 1 coord field'

      blocked = (,) <$> blockedLeft <*> blockedRight
      in case blocked of
           Nothing -> (field'', False)
           Just (minX, maxX) -> let
             toWater = Map.fromList (map (\x -> ((x,y), Water)) [minX..maxX])
             in (toWater <> field'', True)

score s = let
  d = Text.unpack $ display s
  in length (filter (\v -> v == '~' || v == '|') d)
  
score' s = let
  d = Text.unpack $ display s
  in length (filter (\v -> v == '~') d)

-- 29045 is too low!
-- 29048 is too low!

-- * FIRST problem
day :: _ -> Int
day = score . flow

-- * SECOND problem
day' :: _ -> Int
day' = score' . flow

-- * Tests

example = Map.fromSet (const Clay) $ unsafeParse parseContent [here|
x=495, y=2..7
y=7, x=495..501
x=501, y=3..7
x=498, y=2..4
x=506, y=1..2
x=498, y=10..13
x=504, y=10..13
y=13, x=498..504|]

example2 = Map.fromSet (const Clay) $ unsafeParse parseContent [here|
y=4, x=499..501
x=499, y=2..3
x=501, y=2..3
y=6, x=498..502
x=497, y=0..6
|]
  
test :: Spec
test = do
 describe "works" $ do
   it "on first star" $ do
     day fileContent `shouldBe` 29063
   it "on second star" $ do
     day' fileContent `shouldBe` 23811
