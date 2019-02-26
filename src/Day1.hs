module Day1 where

import Utils hiding (parseNumber)

import Text.Megaparsec
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.Char

import qualified Data.Set as Set

parseNumber :: Parser Int
parseNumber = do
  sign <- choice [identity <$ char '+', negate <$ char '-']

  sign <$> decimal

fileContent :: Text
fileContent = $(getFile)

content = unsafeParse (parseNumber `sepBy` "\n") fileContent

-- * Generics


-- 14h22

-- * FIRST problem
day :: [Int] -> Int
day ints = sum ints


-- 569 is too low

-- 14h38: learn to read!
  
-- * SECOND problem
day' :: [Int] -> Maybe Int
day' ints = let
  go (x:xs) v seen
    | v `Set.member` seen = Just v
    | otherwise = go xs (v + x) (Set.insert v seen)
  
  in go (cycle ints) 0 Set.empty

  

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
     day content `shouldBe` 569
   it "on second star" $ do
     day' content `shouldBe` Just 77666
