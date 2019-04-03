module Day11 where

import Utils

import qualified Data.Matrix as Matrix

fileContent :: Int
fileContent = 8979

-- start 20h01 -> 20:13 -> 20:26 (with a solution which do not converge)

-- * Generics
fullCellPower :: Int -> (Int, Int) -> Int
fullCellPower gridSerialNumber (x, y) = let
  rackId = x + 10
  powerLevel = ((rackId * y + gridSerialNumber) * rackId) `mod` 1000 `div` 100 - 5
  in powerLevel

summedAreaGrid :: Int -> Matrix.Matrix Int
summedAreaGrid gsn = m
 where
  m = Matrix.matrix 300 300 $ \(y, x) -> bigI (x, y)

  foo (x, y) = fromMaybe 0 (Matrix.safeGet y x m)

  bigI (x, y) = fullCellPower gsn (x, y) + foo (x, y - 1) + foo (x - 1, y) - foo (x - 1, y - 1)

bli m (x, y) = fromMaybe 0 (Matrix.safeGet y x m)

grid :: Int -> [((Int, Int), Int)]
grid gridSerialNumber = do
  let sag = summedAreaGrid gridSerialNumber

  y <- [1..298]
  x <- [1..298]

  let xmax = x + 2
      ymax = y + 2

      power = bli sag (x - 1, y - 1) + bli sag (xmax, ymax) - bli sag (x - 1, ymax) - bli sag (xmax, y - 1)

  pure ((x, y), power)

grid2 :: Int -> [((Int, Int, Int), Int)]
grid2 gridSerialNumber = do
  let sag = summedAreaGrid gridSerialNumber

  size <- [1 .. 300]

  y <- [1..(300 - size - 1)]
  x <- [1..(300- size - 1)]

  let xmax = x + size - 1
      ymax = y + size - 1

      power = bli sag (x - 1, y - 1) + bli sag (xmax, ymax) - bli sag (x - 1, ymax) - bli sag (xmax, y - 1)

  pure ((x, y, size), power)

-- * FIRST problem
day :: Int -> (Int, Int)
day gsn = fst $ maximumBy (comparing snd) (grid gsn)

-- * SECOND problem
day' gsn = fst $ iterativeMax (grid2 gsn)

iterativeMax :: [((Int, Int, Int), Int)] -> ((Int, Int, Int), Int)
iterativeMax = foldl' nextMax (undefined, -10000)
  where nextMax old@(_, oldV) new@(_, newV)
          | newV > oldV = new
          | otherwise = old

-- * Tests

test :: Spec
test = do
  describe "woks" $ do
    it "on first star" $ do
      day fileContent `shouldBe` (33,34)
    it "on second star" $ do
      day' fileContent `shouldBe` (235, 118, 14)
