{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE EmptyCase #-}
module Day13 where

import Utils

import Text.Megaparsec
import Text.Megaparsec.Char

import qualified Data.Text as Text

import qualified Data.Map as Map

-- start: 03:21 -> 4h45 -> 5h05
data Direction = North | East | South | West
  deriving (Show, Bounded, Enum)

data Side = Slash | Cross | BackSlash | Useless Dir
  deriving (Show)

data CrossOption = CrossLeft | CrossStraight | CrossRight
  deriving (Enum, Bounded, Show)

data Case
  = Motion Dir
  | Cart Direction
  | Turn Side
  | Empty
  deriving Show

data Dir = Vertical | Horizontal deriving (Show)

fileContent :: Game
fileContent = eval $(getFile)

parseCase :: Parser Case
parseCase = choice
  [ char '>' $> Cart East
  , char '<' $> Cart West
  , char 'v' $> Cart South
  , char '^' $> Cart North
  , char '|' $> Motion Vertical
  , char '-' $> Motion Horizontal
  , char '+' $> Turn Cross
  , char '/' $> Turn Slash
  , char '\\' $> Turn BackSlash
  , char ' ' $> Empty
  ]

parser :: Parser [[Case]]
parser = Text.Megaparsec.many parseCase `sepBy` "\n"

data Game = Game
  { carts :: Map (Int, Int) (Direction, CrossOption)
  , grid :: Map (Int, Int) Side
  }
  deriving (Show)

eval t = let grid = unsafeParse parser t
             gridWithInfos  = do
               (y, l) <- zip [0..] grid
               (x, c) <- zip [0..] l
               pure ((x, y), c)
         in foldl' addToGame (Game Map.empty Map.empty) gridWithInfos

addToGame :: Game -> ((Int, Int), Case) -> Game
addToGame g@(Game{carts, grid}) (coord, c) = case c of
  Motion d -> Game carts (Map.insert coord (Useless d) grid)
  Empty -> g
  Turn d -> Game carts (Map.insert coord d grid)
  Cart d -> Game (Map.insert coord (d, CrossLeft) carts) (Map.insert coord (Useless (directionToDir d)) grid)

directionToDir West = Horizontal
directionToDir East = Horizontal
directionToDir South = Vertical
directionToDir North = Vertical

moveACart :: (Int, Int) -> (Direction, CrossOption) -> Maybe Side -> ((Int, Int), (Direction, CrossOption))
moveACart pos (direction, co) Nothing = (deltaPos pos direction, (direction, co))
moveACart pos (direction, co) (Just side) = deltaSide pos direction side co

deltaSide (x, y) direction side co = (deltaPos (x, y) newDir, (newDir, newCross))
  where
    (newDir, newCross) = case (direction, side) of
      (d, Useless _) -> (d, co)
      (North, Slash) -> (East, co)
      (North, BackSlash) -> (West, co)
      (East, Slash) -> (North, co)
      (East, BackSlash) -> (South, co)
      (West, Slash) -> (South, co)
      (West, BackSlash) -> (North, co)
      (South, Slash) -> (West, co)
      (South, BackSlash) -> (East, co)

      (d, Cross) -> handleCross d co

deltaPos (x, y) = \case
  North -> (x, y - 1)
  South -> (x, y + 1)
  East -> (x + 1, y)
  West -> (x - 1, y)

handleCross :: Direction -> CrossOption -> (Direction, CrossOption)
handleCross dir cross = (turn dir cross, succWrap cross)

turn :: Direction -> CrossOption -> Direction
turn d CrossStraight = d
turn d CrossLeft = predWrap d
turn d CrossRight = succWrap d

displayGame (Game carts grid) =
  let
    maxX = maximum (map fst (Map.keys grid))
    maxY = maximum (map snd (Map.keys grid))
  in Text.unlines $ do
    y <- [0..maxY]

    pure . Text.pack $ do
      x <- [0..maxX]

      pure $ case Map.lookup (x, y) carts of
        Nothing -> case Map.lookup (x, y) grid of
          Nothing -> ' '
          Just (Useless Vertical) -> '|'
          Just (Useless Horizontal) -> '-'
          Just Slash -> '/'
          Just BackSlash -> '\\'
          Just Cross -> '+'
        Just (d, _) -> case d of
          South -> 'v'
          North -> '^'
          East -> '>'
          West -> '<'

oneStep :: _ -> Game -> Either (Int, Int) Game
oneStep handleColision game =
  let
    sortedCarts :: [((Int, Int), (Direction, CrossOption))]
    sortedCarts = sortBy (comparing (swap . fst)) (Map.toList (carts game))
  in foldM (moveCart handleColision) game sortedCarts 

moveCart :: _ -> Game -> ((Int, Int), (Direction, CrossOption)) -> Either (Int, Int) Game
moveCart handleColision game@(Game carts grid) (coord, move)
  | not $ coord `Map.member` carts = Right game -- short circuit, to check that the cart did not explode
  | otherwise = 
  let
    (newCartPos, newMove) = moveACart coord move (Map.lookup coord grid)
  in if newCartPos `Map.member` carts
     then handleColision newCartPos coord game
     else Right $ Game (Map.insert newCartPos newMove $ Map.delete coord carts) grid

handleColisionStop newCartPos _ _ = Left newCartPos

handleColisionExplode newCartPos coord game = let
  newCarts = Map.delete coord $ Map.delete newCartPos (carts game)
  in Right $ Game newCarts (grid game)


findFailure :: _ -> Game -> (Int, Int)
findFailure hf g = case oneStep hf g of
  Left r -> r
  Right g' -> case Map.keys (carts g') of
    [x] -> x
    [] -> undefined
    _ -> findFailure hf g'

-- * Generics
example = eval [here|/->-\        
|   |  /----\
| /-+--+-\  |
| | |  | v  |
\-+-/  \-+--/
  \-----/   |]


example2 = eval [here|/>-<\  
|   |  
| /<+-\
| | | v
\>+</ |
  |   ^
  \<->/|]
  
-- * FIRST problem
day = findFailure handleColisionStop

-- * SECOND problem
day' = findFailure handleColisionExplode

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
