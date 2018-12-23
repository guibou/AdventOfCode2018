module Day18 where

-- start 14h19. pause at 25. restart 31 -> 15h08

import Text.Megaparsec
import Text.Megaparsec.Char

import Utils

import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Set as Set

default (Int)

fileContent :: Map (Int, Int) Item
fileContent = toField $ unsafeParse parseField $(getFile)

-- * Generics

data Item = Tree | LumberYard
  deriving (Show, Ord, Eq)

parseItem :: Parser (Maybe Item)
parseItem = choice [
  char '.' $> Nothing,
  char '|' $> Just Tree,
  char '#' $> Just LumberYard
  ]

parseField = Text.Megaparsec.many parseItem `sepBy` "\n"

toField grid = Map.fromList $ do
  (y, l) <- zip [0..] grid
  (x, c) <- zip [0..] l

  case c of
    Nothing -> []
    Just c' -> pure ((x, y), c')

display size field = Text.unlines $ do
  y <- [0..(size - 1)]

  pure $ Text.pack $ do
    x <- [0..(size - 1)]

    pure $ case Map.lookup (x, y) field of
      Nothing -> '.'
      Just Tree -> '|'
      Just LumberYard -> '#'

step :: Int -> Map (Int, Int) Item -> Map (Int, Int) Item
step size m = Map.fromList $ do
  x <- [0..(size - 1)]
  y <- [0..(size - 1)]

  let
    coord = (x, y)
    curValue = Map.lookup coord m

    adjacency = Map.fromListWith (+) $ do
      dx <- [-1 .. 1]
      dy <- [-1 .. 1]

      guard $ (dx, dy) /= (0,0)
      case Map.lookup (x + dx, y + dy) m of
        Nothing -> []
        Just v -> pure (v, 1)

  case curValue of
    Nothing -> if Map.findWithDefault 0 Tree adjacency >= 3
               then pure (coord, Tree)
               else []
    Just Tree -> if Map.findWithDefault 0 LumberYard adjacency >= 3
                 then pure (coord, LumberYard)
                 else pure (coord, Tree)
    Just LumberYard -> if Map.findWithDefault 0 LumberYard adjacency >= 1 && Map.findWithDefault 0 Tree adjacency >= 1
                       then pure (coord, LumberYard)
                       else []

score (Map.elems -> m) = length (filter (==Tree) m) * length (filter (==LumberYard) m)

findFixPoint step i = go i (Map.empty) 0
  where
    go val seen iter = case Map.lookup val seen of
      Just v -> (val, iter, v)
      Nothing -> go (step val) (Map.insert val iter seen) (iter + 1)

advancedIter n step val
  | n < startFixpoint = applyN n step val
  | n > startFixpoint = applyN toDo step atFixPoint
  where
    (atFixPoint, startFixpoint, loopFixpoint) = findFixPoint step val
    toDo = (n - startFixpoint) `mod` (startFixpoint - loopFixpoint)
                         
-- * FIRST problem
day :: _ -> Int
day = score . applyN 10 (step 50)

-- * SECOND problem
day' :: _ -> Int
day' = score . advancedIter 1000000000 (step 50)

-- * Tests
example = toField $ unsafeParse parseField [here|.#.#...|#.
.....#|##|
.|..|...#.
..|#.....#
#.#|||#|#|
...#.||...
.|....|...
||...#|.#|
|.||||..|.
...#.|..|.|]


test :: Spec
test = do
 describe "woks" $ do
   it "on first star" $ do
     day fileContent `shouldBe` 466125
   it "on second star" $ do
     day' fileContent `shouldBe` 207998
