module Day11 where

import Utils

fileContent :: Int
fileContent = 8979

-- start 20h01 -> 20:13 -> 20:26 (with a solution which do not converge)

-- * Generics
fullCellPower :: Int -> (Int, Int) -> Int
fullCellPower gridSerialNumber (x, y) = let
  rackId = x + 10
  powerLevel = ((rackId * y + gridSerialNumber) * rackId) `mod` 1000 `div` 100 - 5
  in powerLevel

grid :: Int -> [((Int, Int), Int)]
grid gridSerialNumber = do
  y <- [1..298]
  x <- [1..298]

  let power = sum $ do
        dx <- [0..2]
        dy <- [0..2]
        pure $ fullCellPower gridSerialNumber (x + dx, y + dy)

  pure ((x, y), power)

grid2 :: Int -> [((Int, Int, Int), Int)]
grid2 gridSerialNumber = do
  size <- [1 .. 300]

  y <- [1..(300 - size - 1)]
  x <- [1..(300- size - 1)]

  let power = sum $ do
        dx <- [0..(size - 1)]
        dy <- [0..(size - 1)]
        pure $ fullCellPower gridSerialNumber (x + dx, y + dy)

  pure ((x, y, size), power)

-- * FIRST problem
day :: Int -> (Int, Int)
day gsn = fst $ maximumBy (comparing snd) (grid gsn)

-- * SECOND problem
day' gsn = fst $ iterativeMax (grid2 gsn)

iterativeMax :: [((Int, Int, Int), Int)] -> ((Int, Int, Int), Int)
iterativeMax = foldl' nextMax (undefined, -10000)
  where nextMax old@(oldC, oldV) new@(newC, newV)
          | newV > oldV = traceShowId new
          | otherwise = old

-- * Tests

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
