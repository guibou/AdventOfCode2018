{-# LANGUAGE NamedFieldPuns #-}
module Day24 where

import Utils hiding (option)

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import qualified Data.Map as Map

import Control.Monad.State as S

type ParserI t = ParsecT Void Text (S.State Int) t

unsafeRun s = either (panic .show) identity (evalState (runParserT parseAll "foo" s) 0)

parseLine kind = do
  id <- get
  modify (+1)
  
  nbUnits <- decimal
  " units each with "
  hp <- decimal
  " hit points "
  (immunities, weaknesses) <- option ([], []) $ do
    "("
    res <- choice [
      Text.Megaparsec.try (flip (,) <$> parseWeak <*> ("; " *> parseImmune))
      , Text.Megaparsec.try ((,) <$> parseImmune <*> ("; " *> parseWeak))
      , (,[]) <$> parseImmune
      , ([],) <$> parseWeak
                  ]
    ") "
    pure res
    
  "with an attack that does "
  attack <- decimal
  " "
  attackType <- parseAttackType
  " damage at initiative "
  initiative <- decimal

  pure $ Units {id, kind, nbUnits, hp, immunities, weaknesses, attack, attackType, initiative}

parseAttackType :: ParserI AttackType
parseAttackType = choice [
  "cold" $> Cold
  , "radiation" $> Radiation
  , "slashing" $> Slashing
  , "bludgeoning" $> Bludgeoning
  , "fire" $> Fire
  ]

data AttackType
  = Cold
  | Radiation
  | Slashing
  | Bludgeoning
  | Fire
  deriving (Show, Eq, Ord)

parseImmune = "immune to " *> parseAttackType `sepBy` ", "
parseWeak = "weak to " *> parseAttackType `sepBy` ", "

parseAll = do
  "Immune System:\n"
  immunes <- parseLine Immune `sepEndBy` "\n"
  "\nInfection:\n"
  infection <- parseLine Infection `sepEndBy` "\n"

  pure (Map.fromList $ map (\u -> (id u, u)) $ (immunes ++ infection))

data Kind = Immune | Infection
  deriving (Show, Eq, Ord)

data Units = Units
  { nbUnits :: Int
  , hp :: Int
  , immunities :: [AttackType]
  , weaknesses :: [AttackType]
  , attack :: Int
  , attackType :: AttackType
  , initiative :: Int
  , kind :: Kind
  , id :: Int
  } deriving (Show, Eq, Ord)

fileContent = unsafeRun $(getFile)

-- * Generics
-- start 20h33. End of parser at 20h57 -- restart at 23h21. Stop at 23h23

effectivePower Units{nbUnits, attack} = nbUnits * attack

groupOrder :: [Units] -> [Units]
groupOrder = sortBy (comparing (Down . effectivePower) <> comparing (Down . initiative))

realPower :: Units -> Units -> Int
realPower unit@Units{attackType} other
  | attackType `elem` immunities other = 0
  | attackType `elem` weaknesses other = 2 * effectivePower unit
  | otherwise = effectivePower unit

pickMostEffectiveAgainst :: Units -> [Units] -> Map Units Units -> Maybe Units
pickMostEffectiveAgainst currentGroup groups attacked = let
  otherArmy = filter (\g -> kind g /= kind currentGroup && realPower currentGroup g > 0 && not (g `Map.member` attacked)) groups
  in case otherArmy of
       [] -> Nothing
       _ -> Just $ maximumBy (comparing (realPower currentGroup) <> comparing effectivePower <> comparing initiative) otherArmy

targetSelection :: [Units] -> Map Units Units
targetSelection units = let
  order = groupOrder units

  affect :: Map Units Units -> Units -> Map Units Units
  affect attacked attacker = case pickMostEffectiveAgainst attacker units attacked of
    Nothing -> attacked
    Just v -> Map.insert v attacker attacked
    
  in Map.fromList . map swap . Map.toList $ foldl' affect Map.empty order

step :: Map Int Units -> Map Int Units
step units = let
  affectations = targetSelection (Map.elems units)
  fightingOrder = sortBy (comparing (Down . initiative)) (Map.elems units)
  in foldl' (flip (fight affectations)) units fightingOrder

fight :: Map Units Units -> Units -> Map Int Units -> Map Int Units
fight maping attacker units = case Map.lookup attacker maping of
  Nothing -> units
  Just defender
    | newNbUnits > 0 -> Map.insert (id defender) (defender { nbUnits = newNbUnits }) units
    | otherwise -> Map.delete (id defender) units
    where
      damages = maybe 0 (\realAttacker -> realPower realAttacker defender) (Map.lookup (id attacker) units)
      defenderHp = maybe 0 hp (Map.lookup (id defender) units)

      unitLost = damages `div` defenderHp
      newNbUnits = (maybe 0 nbUnits (Map.lookup (id defender) units)) - unitLost

fastDisplay :: Map Int Units -> IO ()
fastDisplay units = mapM_ print (Map.elems units)

game units
  | units' == units = units
  | otherwise = game units'
  where units' = step units

-- * FIRST problem
day :: _ -> Int
day = sum . map nbUnits . game

winner units = let
  mapImmune = Map.filter (\u -> kind u == Immune) units
  mapInfect = Map.filter (\u -> kind u == Infection) units
  in if length mapImmune /= 0 && length mapInfect /= 0
     then Nothing
     else if length mapImmune == 0
          then Just Infection
          else Just Immune

-- * SECOND problem
boost boostValue units = case kind units of
  Immune -> units {attack = attack units + boostValue}
  _ -> units

boostAll unitss boostValue = map (boost boostValue) unitss

chien :: _ -> Maybe (Int, _)
chien toto = find (\(_, g) -> winner g == Just Immune) $ (zip [0..] $ (map (game . (boostAll toto)) [0..]))

day' :: _ -> Int
day' = sum . map nbUnits . snd . fromMaybe (panic "da") . chien

-- 6124 is too low

example = unsafeRun [here|Immune System:
17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2
989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3

Infection:
801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1
4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4|]

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
     day fileContent `shouldBe` 26277
   it "on second star" $ do
     day' fileContent `shouldBe` 8812
