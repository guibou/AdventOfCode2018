module Day9 where

import Utils

import qualified Data.Map as Map

-- start 11h20

fileContent :: (Int, Int)
fileContent = (463, 71787)

-- * Generics
data ListZipper = ListZipper [Int] [Int]
  deriving (Show)

data Game = Game (Map Int Int) ListZipper
  deriving (Show)

startGame = Game (Map.empty) (ListZipper [] [0])

emptyZip = ListZipper [] [0]
insert i (ListZipper l v) = ListZipper l (i:v)
read (ListZipper l (x:_)) = x

toLeft (ListZipper l (x:xs)) = ListZipper (x:l) xs
toLeft (ListZipper l []) = toLeft (ListZipper [] (reverse l))

toRight (ListZipper (x:xs) l) = ListZipper xs (x:l)
toRight (ListZipper [] l) = toRight (ListZipper (reverse l) [])

display :: _ -> Text
display (ListZipper l (x:xs)) = show (reverse l) <> show x <> show xs

pop (ListZipper l (x:xs)) = (x, ListZipper l xs)
pop (ListZipper l []) = pop (ListZipper [] (reverse l))


insertMarble playerCount m (Game score zipper)
  | m `mod` 23 /= 0 = Game score (insert m $ toLeft (toLeft zipper))
  | otherwise = let newMarble = (toRight . toRight . toRight . toRight . toRight . toRight . toRight) zipper
                    (v, newMarble') = pop newMarble
                in Game (Map.insertWith (+) (m `mod` playerCount)  (m + v) score) newMarble'

game nPlayers nMarbles = foldl' (flip (insertMarble nPlayers)) startGame [1..nMarbles]

maxScore (Game score _) = maximum (Map.elems score)

-- first start 11h47
-- first start 11h48

-- * FIRST problem
day :: _ -> Int
day = maxScore . uncurry game

-- * SECOND problem
day' :: _ -> Int
day' = day . map (*100)

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
     day fileContent `shouldBe` 396136
   it "on second star" $ do
     day' fileContent `shouldBe` 3183301184
