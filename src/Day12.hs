module Day12 where

import Utils

import Text.Megaparsec
import Text.Megaparsec.Char

import qualified Data.Set as Set
import qualified Data.Bits as Bits
import qualified Data.Vector.Unboxed as V


-- start: 23h50 -> 23h14 --> 03h21

fileContent = startIt $ unsafeParse game $(getFile)

plot :: Parser Bool
plot = choice [char '.' $> False
              ,char '#' $> True]

init = "initial state: " *> manyTill plot (char '\n')

rule = (,) <$> (count 5 plot <* " => ") <*> plot

game :: Parser (Int, Integer)
game = do
  i <- init
  "\n"
  rules <- rule `sepBy` "\n"

  let theSet = foldl' (Bits.setBit) 0 (map (toBits . fst) (filter snd rules))
  pure (theSet, toBits i)

data Status = Status !Int !Integer
  deriving (Show)

toBits l = foldl' s 0 (zip [0..] l)
  where
    s i (idx, True) = Bits.setBit i idx
    s i _ = i

main :: IO ()
main = do
  print $ day'Bench fileContent

-- * Generics
step :: Int -> Status -> Status
step dict (Status decal status) = purge (decal-3) (go 0 0 (Bits.shiftL status 5))
  where
    go :: Integer -> Int -> Integer -> Integer
    go !res !idx !i
      | i == 0 = res
      | testBit dict ((fromIntegral i Bits..&. 0b11111)) = go (Bits.setBit res idx) (idx + 1) (Bits.shiftR i 1)
      | otherwise = go res (idx + 1) (Bits.shiftR i 1)

{-# INLINE purge #-}
purge :: Int -> Integer -> Status
purge d i
  | not $ Bits.testBit i 0 = purge (d + 1) (Bits.shiftR i 1)
  | otherwise = Status d i

-- * FIRST problem
day :: _ -> Int
day = score . job 20

job gen (dict, start) = applyN gen (step dict) start

jobHeuristic gen (dict, start) = let
  heuristicStart = 100
  resPartial = applyN heuristicStart (step dict) start
  in (gen - heuristicStart) * 80 + score resPartial

day'Bench :: _ -> Int
day'Bench = score . job 1000000
-- * SECOND problem
day' :: _ -> Int
day' = jobHeuristic 50000000000

-- * Tests
display :: Status -> [Char]
display (Status _ v) = map (bool '.' '#') . unfoldr decomp $ v
  where
    decomp 0 = Nothing
    decomp i = Just (i `mod` 2 == 1, i `div` 2)

score :: Status -> Int
score (Status decal l) = go decal l
  where
    go :: Int -> Integer -> Int
    go idx 0 = 0
    go idx i = bool 0 idx (Bits.testBit i 0) + go (idx + 1) (Bits.shiftR i 1)

startIt (dict, start) = (dict, Status 0 start)

example = startIt $ unsafeParse game [here|initial state: #..#.#..##......###...###

...## => #
..#.. => #
.#... => #
.#.#. => #
.#.## => #
.##.. => #
.#### => #
#.#.# => #
#.### => #
##.#. => #
##.## => #
###.. => #
###.# => #
####. => #|]

test :: Spec
test = do
 describe "woks" $ do
   it "on first star" $ do
     day fileContent `shouldBe` 3217
   it "on second star" $ do
     day' fileContent `shouldBe` 4000000000866
