module Day23 where

-- start at 11h47 -> 11h57

import Text.Megaparsec

import qualified Data.SBV as SBV

import Utils

fileContent :: Num t => [Bot t]
fileContent = unsafeParse parseContent $(getFile)

data V3 t = V3 { x :: t,  y :: t,  z :: t}
  deriving (Show, Ord, Eq)

parseV3 :: Num t => Parser (V3 t)
parseV3 = V3 <$> parseNumber <*> ("," *> parseNumber) <*> ("," *> parseNumber)
  
data Bot t = Bot {
  position :: V3 t
  , radius :: t }
  deriving (Show)

parseStuff :: Num t => Parser (Bot t)
parseStuff = Bot <$> ("pos=<" *> parseV3) <*> (">, r=" *> parseNumber)

parseContent :: Num t => Parser [Bot t]
parseContent = Text.Megaparsec.some parseStuff

dist (V3 x y z) (V3 x' y' z') = abs (x - x') + abs (y - y') + abs (z - z')

-- * Generics

strongestBot :: Ord t => [Bot t] -> Bot t
strongestBot = maximumBy (comparing radius)

inRange :: (Ord t, Num t) => Bot t -> Bot t -> Bool
inRange Bot{position, radius} (Bot p' _) = dist position p' <= radius

-- * FIRST problem
day :: [Bot Int] -> Int
day bots = let
  strongest = strongestBot bots
  in length (filter (inRange strongest) bots)
-- * SECOND problem

toto :: [Bot SBV.SInteger] -> IO (Maybe Integer)
toto boxs = do
  SBV.LexicographicResult result <- SBV.optimize SBV.Lexicographic $ do
    x <- SBV.sInteger "x"
    y <- SBV.sInteger "y"
    z <- SBV.sInteger "z"

    SBV.maximize "goal" $ sum @[] @SBV.SInteger (map (\b -> SBV.oneIf (dist (V3 x y z) (position b) SBV..<= radius b)) boxs)

    SBV.minimize "dist" $ dist (V3 x y z) (V3 0 0 0)

  pure $ SBV.getModelValue "dist" result

day' :: _ -> IO (Maybe Integer)
day' = toto

example :: [Bot SBV.SInteger]
example = unsafeParse parseContent [here|pos=<10,12,12>, r=2
pos=<12,14,12>, r=2
pos=<16,12,12>, r=4
pos=<14,14,14>, r=6
pos=<50,50,50>, r=200
pos=<10,10,10>, r=5|]

example' :: [Bot SBV.SInteger]
example' = unsafeParse parseContent [here|pos=<5,5,5>, r=14
pos=<5,5,5>, r=14|]
  
-- * Tests

test :: Spec
test = do
 describe "woks" $ do
   it "on first star" $ do
     day fileContent `shouldBe` 730
   it "on second star" $ do
     day' fileContent `shouldReturn` (Just 48202279)
