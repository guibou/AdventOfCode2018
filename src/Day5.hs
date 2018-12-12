module Day5 where

import Utils
import Data.Char
-- start 20h48. first start 20h56.
-- restart for second star: 21h13 -> 21h23 because I'm stupid ;)

import qualified Data.Text as Text

import qualified Data.Set as Set

fileContent :: Text
fileContent = $(getFile)

-- * Generics
react (x:y:xs)
  | x /= y && toLower x == toLower y && toUpper x == toUpper y = react xs
  | otherwise = x:react (y:xs)
react r = r

-- * FIRST problem
day :: Text -> Int
day t = length $ red t

red t = go (Text.unpack t)
  where
    go s
      | s' == s = s
      | otherwise = go s'
      where s' = react s

-- 9084 is too high
-- * SECOND problem
day' :: _ -> _
day' t = let
  small = red t
  in length $ minimumBy (comparing length) (map (red . Text.pack . kill small) ['a'..'z'])
  
kill s char = filter (\x -> toLower x /= char) s


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
