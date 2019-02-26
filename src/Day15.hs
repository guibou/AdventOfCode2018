module Day15 where

import Utils hiding (race)
import qualified Data.Set as Set
import qualified Data.Map as Map

import qualified Data.Text as Text
import qualified Data.List as List


-- start 18h34. stop at 19h34... No motivation at all.
fileContent = toGame' $(getFile)

toGame board = mconcat $ do
  (y, l) <- zip [0..] board
  (x, c) <- zip [0..] l

  let
    coord = Position (x, y)
    s r = Map.singleton coord (Unit (Id (hash coord)) coord startingHp r)

  pure $ case c of
    '#' -> Game (Set.singleton coord) mempty
    '.' -> Game mempty mempty
    'E' -> Game mempty (s Elf)
    'G' -> Game mempty (s Gobelin)
    _ -> panic ("WTF char" <> show c)

startingHp = 200
gobelinAttackPower = 3

-- * Generics
data Race = Gobelin | Elf
  deriving (Show, Eq)

newtype Id = Id Int
  deriving (Show, Eq)

newtype Position = Position (Int, Int)
  deriving stock (Eq, Show, Generic)
  deriving newtype (Hashable)

instance Ord Position where
  compare (Position a) (Position b) = compare (swap a) (swap b)

data Unit = Unit
  { id :: Id
  , position :: Position
  , hp :: Int
  , race :: Race
  }
  deriving (Show, Eq)

data Game = Game
  { walls :: Set Position
  , units :: Map Position Unit
  }
  deriving (Show)

instance Semigroup Game where
 (Game a b) <> (Game a' b') = Game (a <> a') (b <> b')

instance Monoid Game where
  mempty = Game mempty mempty

posX (Position (x, _)) = x
posY (Position (_, y)) = y

displayGame game =
  let
    maxX = posX $ maximumBy (comparing posX) (Map.keysSet (units game) <> (walls game))
    maxY = posY $ maximumBy (comparing posY) (Map.keysSet (units game) <> (walls game))
  in putStrLn $ Text.unlines $ do
    y <- [0..maxY]
    let guys = filter (\(pos, _) -> posY pos == y) (Map.toList (units game))
    pure $ (Text.pack $ do
      x <- [0..maxX]

      let coord = Position (x, y)
      pure $ case Map.lookup coord (units game) of
        Nothing -> if coord `Set.member` (walls game)
                   then '#'
                   else '.'
        Just u -> case race u of
          Gobelin -> 'G'
          Elf -> 'E') <> " " <> showGuys guys

showGuys = show . map (hp . snd)

-- * FIRST problem
runRound :: Int -> Game -> Either Game Game
runRound elfAttackPower game = foldM (unitTurn elfAttackPower) game (roundOrder game)

roundOrder :: Game -> [Id]
roundOrder game = fmap id (Map.elems $ units game)

unitTurn :: Int -> Game -> Id -> Either Game Game
unitTurn elfAttackPower game unitId = case pickUnit game unitId of
  Nothing -> Right game -- unit is dead
  Just unit -> case identifyTargets game unit of
    [] -> Left game -- End Of Game
    enemies -> let
      (newGame, newUnit) = if not $ alreadyInRange unit enemies
        then movePhase game unit enemies
        else (game, unit)
      in Right $ attackPhase elfAttackPower newGame newUnit enemies

alreadyInRange unit enemies = or $ do
  ennemy <- enemies
  pos <- conn4 (position unit)

  pure $ pos == position ennemy

movePhase :: Game -> Unit -> [Unit] -> (Game, Unit)
movePhase game unit ennemis = case findReachables game unit ennemis of
  Nothing -> (game, unit)
  Just ennemy -> let
    newPos = findNewPosInDirection game (position unit) (position ennemy)
    newUnit = unit {position = newPos}
    newGame = game { units = Map.insert newPos newUnit $ Map.delete (position unit) (units game)}
    in (newGame, newUnit)

findNewPosInDirection :: Game -> Position -> Position -> Position
findNewPosInDirection game start end = let
  nextPos = do
    pos <- conn4 start
    guard $ not (isOccupied pos game)
    case distanceToTarget game end pos of
      Nothing -> []
      Just d -> pure (d, pos)

  in
  snd $ minimum nextPos

distanceToTarget :: Game -> Position -> Position -> Maybe Int
distanceToTarget game target start = go 0 (Set.singleton start) (Set.empty)
  where
    go dist currentPoss done
      | null currentPoss = Nothing
      | otherwise =
        let nextPos = Set.fromList $ do
              pos <- Set.toList currentPoss
              pos' <- conn4 pos

              -- We can't go through a wall or in an already visited position
              guard $ not (pos' `Set.member` done) && (not (pos' `Set.member` (walls game)))
              pure pos'
            nextPos' = Set.filter (\p -> not (p `Map.member` (units game))) nextPos
        in if target `Set.member` nextPos
           then Just dist
           else go (dist+1) nextPos' (Set.union done currentPoss)


findReachables :: Game -> Unit -> [Unit] -> Maybe Unit
findReachables game (position -> startingPos) ennemies = go (Set.singleton startingPos) (Set.empty)
  where
    go currentPoss done
      | null currentPoss = Nothing
      | otherwise =
        let nextPos = Set.fromList $ do
              pos <- Set.toList currentPoss
              pos' <- conn4 pos

              -- We can't go through a wall or in an already visited position
              guard $ not (pos' `Set.member` done) && (not (pos' `Set.member` (walls game)))
              pure pos'

            -- we cannot go through another player
            nextPos' = Set.filter (\p -> not (p `Map.member` (units game))) nextPos

            reachedTargets = do
              enemy <- ennemies
              guard $ position enemy `Set.member` nextPos
              pure enemy
        in case reachedTargets of
          [] -> go nextPos' (Set.union done currentPoss)
          _ -> Just $ minimumBy (comparing position) reachedTargets

attackPhase :: Int -> Game -> Unit -> [Unit] -> Game
attackPhase elfAttackPower game unit enemies = let
  adjacentUnits = filter isAdjacent enemies
  isAdjacent enemy = position enemy `List.elem` (conn4 (position unit))

  attackPower Gobelin = gobelinAttackPower
  attackPower Elf = elfAttackPower
  in case adjacentUnits of
       [] -> game
       _ -> let
         currentEnemy = minimumBy (comparing hp <> comparing position) adjacentUnits
         newEnemy = currentEnemy {hp = hp currentEnemy - attackPower (race unit)}

         in game { units = if hp newEnemy <= 0
                           then Map.delete (position newEnemy) (units game)
                           else Map.insert (position newEnemy) newEnemy (units game) }

identifyOpenSquares :: Game -> [Unit] -> [Position]
identifyOpenSquares game units = do
  unit <- units
  pos <- conn4 (position unit)

  guard $ not (isOccupied pos game)
  pure pos

isOccupied :: Position -> Game -> Bool
isOccupied pos game = (pos `Set.member` (walls game)) || (pos `Map.member` (units game))

isWalkable :: Position -> Game -> Bool
isWalkable pos game = (pos `Set.member` (walls game)) || (pos `Map.member` (units game))

conn4 :: Position -> [Position]
conn4 (Position (x, y)) = map Position [
  (x - 1, y),
  (x + 1, y),
  (x, y - 1),
  (x, y + 1)
  ]

identifyTargets :: Game -> Unit -> [Unit]
identifyTargets game attacker = filter (\u -> race u /= race attacker) (Map.elems (units game))

pickUnit :: Game -> Id -> Maybe Unit
pickUnit g unitId = List.find (\u -> id u == unitId) (Map.elems (units g))
  
day :: Game -> Int
day game = let
  elfAttackPower = 3
  (finalGame, numberRounds) = fixpoint elfAttackPower game
  in numberRounds * sum (map hp (Map.elems (units finalGame)))

-- * SECOND problem
day' :: _ -> Int
day' game = let
  (finalGame, numberRounds) = fixpoint2 game
  in numberRounds * sum (map hp (Map.elems (units finalGame)))


default (Int)

fixpoint :: Int -> Game -> (Game, Int)
fixpoint elfAttackPower game = go 0 game
  where
    go count game = case runRound elfAttackPower game of
      Right game' -> go (count + 1) game'
      Left game' -> (game', count)
      
countElf :: Game -> Int
countElf g = length $ (filter (\u -> race u == Elf)) (Map.elems $ units g)

fixpoint2 :: Game -> (Game, Int)
fixpoint2 game = go 3 200 -- 200 attack power is enough to oneshot everything
  where
    startElf = countElf game
    go a b
      | a == b = fixpoint a game
      | (a + 1) == b = fixpoint b game
      | otherwise = let
          c = (a + b) `div` 2
          (endgame, _) = fixpoint c game
          in if countElf endgame == startElf
             then go a c
             else go c b


-- ANSWERED: 232380 that's too high !
-- first correct answer: 229798

--- star2: 54366 too high!
--- : 52972 is OK

-- TODO: the ending method is broken

-- * Tests

simpleGame = toGame' $ [here|#######
#.G...#
#...EG#
#.#.#G#
#..G#E#
#.....#
#######|]

toGame' = toGame . map Text.unpack . Text.lines


simpleMoves = toGame' [here|#########
#G..G..G#
#.......#
#.......#
#G..E..G#
#.......#
#.......#
#G..G..G#
#########|]


othersGames = [
  (toGame' [here|#######
#G..#E#
#E#E.E#
#G.##.#
#...#E#
#...E.#
#######|], 36334),

  (toGame' [here|#######
#E..EG#
#.#G.E#
#E.##E#
#G..#.#
#..E#.#
#######|], 39514),
  (toGame' [here|#######
#E.G#.#
#.#G..#
#G.#.G#
#G..#.#
#...E.#
#######|], 27755),
  (toGame' [here|#######
#.E...#
#.#..G#
#.###.#
#E#G#G#
#...#G#
#######|], 28944),
  (toGame' [here|#########
#G......#
#.E.#...#
#..##..G#
#...##..#
#...#...#
#.G...G.#
#.....G.#
#########|], 18740)]

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day simpleGame `shouldBe` 27730
    it "of second star" $ do
      day' simpleGame `shouldBe` 4988
  describe "woks" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 229798
    it "on second star" $ do
      day' fileContent `shouldBe` 52972
