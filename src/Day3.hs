module Day3 where

import Utils

import qualified Data.Map as Map
import qualified Data.Set as Set

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

fileContent :: Text
fileContent = $(getFile)

-- 14h57

-- * Generics
data Claim = Claim {
  id :: Int,
  padding :: (Int, Int),
  size :: (Int, Int)
  }
  deriving (Show)

parseClaim :: Parser Claim
parseClaim = do
  char '#'
  id <- decimal
  string " @ "
  padding <- (,) <$> decimal <*> ("," *> decimal)
  string ": "
  size <- (,) <$> decimal <*> ("x" *> decimal)

  pure $ Claim id padding size

content = unsafeParse (parseClaim `sepBy` "\n") fileContent

patchToValue :: Claim -> [(Int, (Int, Int))]
patchToValue claim = do
  let (sx, sy) = size claim
  let (px, py) = padding claim
  dx <- [0 .. sx - 1]
  dy <- [0 .. sy - 1]

  pure (id claim, (px + dx, py + dy))

-- * FIRST problem
dayMap :: [Claim] -> Map (Int, Int) [Int]
dayMap claims = Map.fromListWith (++) (map (\(id, pos) -> (pos,[id])) patchs)
  where
    patchs = mconcat (map patchToValue claims)

day claims = length (filter (\x -> length x >1) overridePatchs)
  where
    overridePatchs = Map.elems (dayMap claims)

-- 15:08
-- 15h16
-- * SECOND problem
day' :: [Claim] -> _
day' claims = go patchs alls
  where
    go [] res = res
    go (x:xs) alls
      | length x == 1 = go xs alls
      | otherwise = go xs (alls `Set.difference` (Set.fromList x))
    
    alls = Set.fromList [1 .. length claims]
    patchs = Map.elems (dayMap claims)

-- * Tests

test :: Spec
test = do
  describe "woks" $ do
    it "on first star" $ do
      day content `shouldBe` 116140
    it "on second star" $ do
      day' content `shouldBe` (Set.singleton 574)
