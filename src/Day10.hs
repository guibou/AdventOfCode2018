module Day10 where

import Utils

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Text as Text
import Text.Megaparsec.Char.Lexer

import qualified Data.Set as Set

fileContent :: Text
fileContent = $(getFile)

data Vec2 = Vec2 {
  x:: Int, y :: Int} deriving (Show, Ord, Eq)

data Point = Point
  { position :: Vec2
  , velocity :: Vec2
  }
  deriving (Show)

-- 19h21

parseVec2 = do
  Utils.lexeme (char '<')
  x <- Utils.lexeme number
  Utils.lexeme (",")
  y <- Utils.lexeme number
  char '>'
  pure (Vec2 x y)

number :: Parser Int
number = do
  sign <- Text.Megaparsec.option identity (char '-' $> negate)
  d <- decimal
  pure (sign d)

parsePoint = do
  "position="
  p <- parseVec2
  " velocity="
  v <- parseVec2
  pure (Point p v)

parseContent = parsePoint `sepBy` "\n"

content = unsafeParse parseContent fileContent
-- * Generics
(Vec2 x y) .+. (Vec2 dx dy) = Vec2 (x + dx) (y + dy)

tickPoint (Point p v) = Point (p .+. v) v

drawPoints :: [Point] -> Text
drawPoints points = Text.unlines $ do
  let
    positions = map position points
    (minX, maxX) = (minimum $ map x positions, maximum $ map x positions)
    (minY, maxY) = (minimum $ map y positions, maximum $ map y positions)

    pts = Set.fromList positions

  y <- [minY .. maxY]

  pure $ Text.pack $ do
    x <- [minX .. maxX]
    pure (if (Vec2 x y) `Set.member` pts then '#' else '.')

getSurface points = 
  let
    positions = map position points
    (minX, maxX) = (minimum $ map x positions, maximum $ map x positions)
    (minY, maxY) = (minimum $ map y positions, maximum $ map y positions)
  in ((maxX - minX) * (maxY - minY))

step :: [Point] -> [Point]
step = map tickPoint

go :: [Point] -> ([Point], Int)
go pts
  | getSurface newPts > getSurface pts = (pts, 0)
  | otherwise = map (+1) $ go newPts
  where
  newPts = step pts

-- first star: 19h58. second star: 19h59

-- * FIRST problem
day :: [Point] -> IO ()
day = putStrLn . drawPoints . fst . go

-- * SECOND problem
day' :: _ -> Int
day' = snd . go

-- * Tests

example = unsafeParse parseContent [here|position=< 9,  1> velocity=< 0,  2>
position=< 7,  0> velocity=<-1,  0>
position=< 3, -2> velocity=<-1,  1>
position=< 6, 10> velocity=<-2, -1>
position=< 2, -4> velocity=< 2,  2>
position=<-6, 10> velocity=< 2, -2>
position=< 1,  8> velocity=< 1, -1>
position=< 1,  7> velocity=< 1,  0>
position=<-3, 11> velocity=< 1, -2>
position=< 7,  6> velocity=<-1, -1>
position=<-2,  3> velocity=< 1,  0>
position=<-4,  3> velocity=< 2,  0>
position=<10, -3> velocity=<-1,  1>
position=< 5, 11> velocity=< 1, -2>
position=< 4,  7> velocity=< 0, -1>
position=< 8, -2> velocity=< 0,  1>
position=<15,  0> velocity=<-2,  0>
position=< 1,  6> velocity=< 1,  0>
position=< 8,  9> velocity=< 0, -1>
position=< 3,  3> velocity=<-1,  1>
position=< 0,  5> velocity=< 0, -1>
position=<-2,  2> velocity=< 2,  0>
position=< 5, -2> velocity=< 1,  2>
position=< 1,  4> velocity=< 2,  1>
position=<-2,  7> velocity=< 2, -2>
position=< 3,  6> velocity=<-1, -1>
position=< 5,  0> velocity=< 1,  0>
position=<-6,  0> velocity=< 2,  0>
position=< 5,  9> velocity=< 1, -2>
position=<14,  7> velocity=<-2,  0>
position=<-3,  6> velocity=< 2, -1>|]

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
