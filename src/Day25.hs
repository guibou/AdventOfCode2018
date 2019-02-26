module Day25 where

-- start 17h31. 17h57

import Utils

import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.UnionFind.ST as UnionFind

fileContent = content $(getFile)

number :: Parser Int
number = do
  sign <- Text.Megaparsec.option identity (negate <$ char '-')
    
  sign <$> decimal

line = do
  [x, y, z, t] <- number `sepBy` ","
  pure (x, y ,z ,t)

content = unsafeParse (line `sepBy` "\n")

dist (x, y, z, t) (x', y', z', t') = abs (x - x') + abs (y - y') + abs (z - z') + abs (t - t')

groups :: [(Int, Int, Int, Int)] -> ST s Int
groups items = do
  points <- mapM UnionFind.fresh items
  let mapping = Map.fromList (zip items points)
  go mapping items

  reprs <- mapM UnionFind.repr points
  descs <- mapM UnionFind.descriptor reprs

  pure $ length (Set.fromList descs)

go _ [] = pure ()
go mapping (x:xs) = do
  mapM_ (testUnion mapping x) xs
  go mapping xs

testUnion :: _ -> (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> ST s ()
testUnion m p0 p1 = do
  let Just d0 = Map.lookup p0 m
  let Just d1 = Map.lookup p1 m

  if dist p0 p1 <= 3
    then UnionFind.union d0 d1
    else pure ()

-- * Generics


-- * FIRST problem
day :: _ -> Int
day content = runST (groups content)

example1 = content [here|
-1,2,2,0
0,0,2,-2
0,0,0,-2
-1,2,0,0
-2,-2,-2,2
3,0,2,-1
-1,3,2,2
-1,0,-1,0
0,2,1,-2
3,0,0,0|]

example2 = content [here|1,-1,0,1
2,0,-1,0
3,2,-1,0
0,0,3,1
0,0,-1,-1
2,3,-2,0
-2,2,0,0
2,-2,0,-1
1,-1,0,-1
3,2,0,2|]

example3 = content [here|1,-1,-1,-2
-2,-2,0,1
0,2,1,3
-2,3,-2,1
0,2,3,-2
-1,-1,1,-2
0,-2,-1,0
-2,2,3,-1
1,2,2,0
-1,-2,0,-2|]

example0 = content [here|0,0,0,0
3,0,0,0
0,3,0,0
0,0,3,0
0,0,0,3
0,0,0,6
9,0,0,0
12,0,0,0|]
  
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
      day fileContent `shouldBe` 305