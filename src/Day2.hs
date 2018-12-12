module Day2 where

import Utils

import qualified Data.Text as Text
import qualified Data.Map as Map

-- start: 14h39

fileContent :: [Text]
fileContent = Text.lines $(getFile)

-- * Generics
countLetters :: [Char] -> Map Char Int
countLetters l = Map.fromListWith (+) (map (,1) l)

checksum :: Text -> (Int, Int)
checksum t = let
  counts = Map.elems $ countLetters (Text.unpack t)
  in (if 2 `elem` counts then 1 else 0, if 3 `elem` counts then 1 else 0)

-- * FIRST problem
day :: [Text] -> Int
day t = let (resA, resB) = foldl' (\(a, b) (a', b') -> (a + a', b + b')) (0, 0) (map checksum t)
        in resA * resB

-- 14h48
matchTwo :: [Char] -> [Char] -> Maybe [Char]
matchTwo (x:xs) (y:ys)
  | (x == y) = (x:) <$> matchTwo xs ys
  | xs == ys = Just xs
  | otherwise = Nothing

-- 14h56: second star

-- * SECOND problem
day' :: [Text] -> Maybe [Char]
day' t = headMay (go (map Text.unpack t))
  where go (x:xs) = catMaybes (map (matchTwo x) xs) ++ go xs

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
