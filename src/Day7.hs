module Day7 where

import Utils

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import qualified Data.Set as Set
import qualified Data.Map as Map

--start 23h42 -> 23h58 -> 1h07

fileContent :: Text
fileContent = $(getFile)

-- * Generics

parseLine :: Parser (Char, Char)
parseLine = (,) <$> ("Step " *> anyChar <* " must be finished before step ") <*> (anyChar <* " can begin.")
  
parseContent = parseLine `sepBy` "\n"

content :: [(Char, Char)]
content = unsafeParse parseContent fileContent

allKeys :: [(Char, Char)] -> [Char]
allKeys v = concatMap (\(a, b) -> [a, b]) v

depTree :: [(Char, Char)] -> Map Char (Set Char)
depTree l = let
  a = allKeys l
  m :: Map Char (Set Char)
  m = Map.fromListWith (<>) $ map (\(a, b) -> (b, Set.singleton a)) l
  in m <> Map.fromList (map (,Set.empty) a)

pickNext :: Map Char (Set Char) -> Maybe (Char, Map Char (Set Char))
pickNext m
  | null m = Nothing
  | otherwise = let
      l = Map.toList  m
      c = minimum (map fst (filter (null . snd) l))
      in Just (c, map (Set.delete c) (Map.delete c m))

data DepsOrTime = Deps (Set Char) | Time Int | Available
  deriving (Show)

pickNext' :: Int -> Int -> Map Char (DepsOrTime) -> Maybe ([Char], Map Char DepsOrTime)
pickNext' wC deltaT m
  | null m = Nothing
  | otherwise = Just (progress wC deltaT m)

isTime (Time _) = True
isTime _ = False
  
isTimeDone (Time 0) = True
isTimeDone _ = False

progress wC deltaT m =
  let
    l = Map.toList m
    -- pick workers
    c = take wC $ sortBy (comparing fst) (filter (isTime . snd) l)

    numberOfAvailableSeats = wC - length c

    c' = newWorkers deltaT m numberOfAvailableSeats

    c'' = map (\(ch, Time t) -> (ch, Time (t - 1))) (c ++ c')
    newMap = Map.fromList c'' <> m
  in cleanMap deltaT newMap

newWorkers :: Int -> Map Char DepsOrTime -> Int -> [(Char, DepsOrTime)]
newWorkers deltaT m n = let
  availables = take n $ sort $ map fst $ filter (isAvailable . snd) (Map.toList m)
  in map (\c -> (c, Time $ tFunc deltaT c)) availables

isAvailable Available = True
isAvailable _ = False

setAvailable deltaT m a = Map.insert a (tFunc deltaT a) m 


depTree' :: Int -> [(Char, Char)] -> Map Char DepsOrTime
depTree' deltaT l = let
  a = allKeys l
  m :: Map Char DepsOrTime
  m = fmap Deps $ Map.fromListWith (<>) $ map (\(a, b) -> (b, Set.singleton a)) l
  in m <> Map.fromList (map (\c -> (c,Available)) a)


cleanMap :: Int -> Map Char DepsOrTime -> ([Char], Map Char DepsOrTime)
cleanMap deltaT m = let
  l = Map.toList m
  done = sort (map fst (filter (isTimeDone . snd) l))
  newMap = foldl' (flip Map.delete) m done
  newMap' = foldl' (\m' c -> Map.mapWithKey (remove deltaT c) m') newMap done
  in (done, newMap')

tFunc deltaT c = ord c - 65 + deltaT

remove :: Int -> Char -> Char -> DepsOrTime -> DepsOrTime
remove deltaT c key (Deps s) = let ns = Set.delete c s in if null ns
                                                          then Available
                                                               -- Time (tFunc deltaT key)
                                                               
                                                          else Deps ns
remove _ c k t = t

-- * FIRST problem
day :: _ -> Int
day = undefined

-- * SECOND problem
day' e deltaT wC= length (steps wC deltaT (depTree' wC e))
  -- concat $ unfoldr (pickNext' 2 1) (depTree' 1 example)

-- WRONG: JNOSIKYAQUWBREVXGTZFMDHLPC
-- WRONG: 260
-- * Tests

example = unsafeParse parseContent [here|Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.|]

steps wC deltaT input = case pickNext' wC deltaT input of
  Nothing -> []
  Just (res, next) -> do
    res:steps wC deltaT next

test :: Spec
test = do
--   describe "simple examples" $ do
--     it "of first star" $ do
--       day "" `shouldBe` 0
--     it "of second star" $ do
--       day' "" `shouldBe` 0
  describe "woks" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 1228
    it "on second star" $ do
      day' content `shouldBe` 1099
