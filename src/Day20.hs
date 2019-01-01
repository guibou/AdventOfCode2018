module Day20 where

import Utils

import Data.String

import Text.Megaparsec
import Text.Megaparsec.Char

import qualified Data.Text as Text
import qualified Data.Map as Map

-- start 16:01

fileContent = unsafeParse parseFile $(getFile)

data Direction = N | E | W | S
  deriving (Show, Eq, Ord)

parseDirection :: Parser Direction
parseDirection = choice
  [ char 'N' $> N
  , char 'E' $> E
  , char 'W' $> W
  , char 'S' $> S
  ]

data Regex
  = Group [Regex]
  | D Direction
  | Or [Regex]
  | Opt [Direction]
  deriving (Show)

parseFile = "^" *> parseRegex <* "$"

parseRegex = do
  (opt, grp) <- sepBy1OptionalEnding parseGroup "|"

  if opt
    then let ~[Group rs] = grp
             dirs = map (\(D d) -> d) rs
         in pure $ Opt (take (length dirs `div` 2) dirs)
    else case grp of
           [x] -> pure x
           _ -> pure (Or grp)

sepBy1OptionalEnding :: Parser a -> Parser b -> Parser (Bool, [a])
sepBy1OptionalEnding p sep = do
  res <- p
  supWithOptional [res] p sep

supWithOptional :: [a] -> Parser a -> Parser b -> Parser (Bool, [a])
supWithOptional acc p sep = do
  hasSep <- optional sep
  case hasSep of
    Nothing -> pure (False, reverse acc)
    Just _ -> do
      nextItem <- optional p
      case nextItem of
        Nothing -> pure (True, reverse acc)
        Just res -> supWithOptional (res : acc) p sep
    
parseGroup :: Parser Regex
parseGroup = do
  el <- (Text.Megaparsec.some ((choice
                                [ parseSubGroup
                                , D <$> parseDirection
                                ])))
  case el of
    [x] -> pure x
    _ -> pure $ Group el

parseSubGroup :: Parser Regex
parseSubGroup = do
  el <- char '(' *> Text.Megaparsec.some parseRegex <* char ')'
  case el of
    [x] -> pure x
    _ -> pure $ Group el

-- * Generics
shortestPath :: Regex -> Int
shortestPath (D _) = 1
shortestPath (Opt _r) = 0
shortestPath (Group r) = sum (map shortestPath r)
shortestPath (Or r) = maximum (map shortestPath r)

-- * SECOND problem
instance IsString Regex where
  fromString = unsafeParse parseFile . Text.pack

decal N = (0, -1)
decal S = (0, 1)
decal E = (1, 0)
decal W = (-1, 0)

dPrefix d (x, y) = let
  (dx, dy) = decal d
  in (x + dx, y + dy)

walk :: Regex -> _
walk r' = snd $ go r' [(0, 0)] []
 where
   go r prefixs edges = case r of
      (D d) -> let
        newPrefixs = map (dPrefix d) prefixs
        newEdges = zip prefixs newPrefixs
        in (newPrefixs, edges ++ newEdges)
      (Or regex) -> let
        acc = map (\reg -> go reg prefixs []) regex
        in (concatMap fst acc, edges ++ concatMap snd acc)
      (Group regex) -> foldl' f (prefixs, edges) regex
        where f (p, d) reg = go reg p d
      (Opt dirs) -> let
        (_, newEdges) = go (Group (map D dirs)) prefixs edges
        in (prefixs, newEdges)

findPaths :: Regex -> _
findPaths regex = let
  edges = walk regex
  edgesMap = Map.fromListWith (++) (concatMap (\(e0, e1) -> [(e0, [e1]), (e1, [e0])]) edges)
  go _ [] known = known
  go currentDepth prefixs known = let
      (nextPrefixs, nextKnown) = foldl' f ([], known) prefixs 
      f :: ([(Int, Int)], Map (Int, Int) Int) -> (Int, Int) -> ([(Int, Int)], Map (Int, Int) Int)
      f (prefix, known) currentPrefix = case Map.lookup currentPrefix edgesMap of
        Nothing -> (prefix, known)
        Just edges -> foldl' f' (prefix, known) edges

      f' (prefix, known) next = case Map.lookup next known of
        Nothing -> (next : prefix, Map.insert next (currentDepth + 1) known)
        Just _ -> (prefix, known)
      in go (currentDepth + 1) nextPrefixs nextKnown
  in go 0 [(0, 0)] (Map.singleton (0, 0) 0)

-- * FIRST problem
day :: _ -> Int
day = maximum . Map.elems . findPaths

-- * SECOND problem
day' :: _ -> Int
day' = length . filter (>=1000) . Map.elems . findPaths

-- * Tests

test :: Spec
test = do
 describe "woks" $ do
   it "on first star" $ do
     day fileContent `shouldBe` 4025
   it "on second star" $ do
     day' fileContent `shouldBe` 8186
