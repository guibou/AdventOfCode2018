module Day8 where

import Utils

-- start at 11h00
-- both stars at 11h19

fileContent :: [Int]
fileContent = unsafeRead1D $(getFile)

data Tree = Tree [Tree] [Int]
  deriving (Show)

-- * Generics
toTree :: [Int] -> (Tree, [Int])
toTree (childCount:metadataCount:rest) =
  let (trees, rest') = readNSubTrees childCount rest
      metadata = take metadataCount rest'
      rest'' = drop metadataCount rest'
  in (Tree trees metadata, rest'')

readNSubTrees 0 rest = ([], rest)
readNSubTrees n rest = let
  (t, rest') = toTree rest
  (ts, rest'') = readNSubTrees (n -1) rest'
  in (t:ts, rest'')

sumMetadata (Tree subTree metadata) = sum metadata + (sum $ map sumMetadata subTree)

-- * FIRST problem
day :: _ -> Int
day = sumMetadata . fst . toTree

-- * SECOND problem
rootNodeValue (Tree [] metadata) = sum metadata
rootNodeValue (Tree ts metadata) = sum (map getSub metadata)
  where
    getSub n = let n' = n - 1
               in if n' < 0 || n' >= length ts
                  then 0
                  else rootNodeValue (ts `unsafeIndex` n')
  
day' :: _ -> Int
day' = undefined

-- * Tests

example :: [Int]
example = unsafeRead1D "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"

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
