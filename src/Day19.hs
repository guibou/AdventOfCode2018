module Day19 where

import Utils

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import qualified Data.Map as Map
import qualified Data.Vector as Vector

import Day16

default (Int)
-- start 15h13. First start 15h40

fileContent = unsafeParse parseProgram $(getFile)

parseIP = "#ip " *> (decimal :: Parser Int)

parseInstr = do
  i <- manyTill anyChar (char ' ')

  let
    ins = case i of
      "gtir" -> OpCodeIR GTir 
      "eqir" -> OpCodeIR EQir 
      "seti" -> OpCodeIR SETi 
      
      "gtrr" -> OpCodeRR GTrr 
      "eqrr" -> OpCodeRR EQrr 
      "addr" -> OpCodeRR ADDr 
      "mulr" -> OpCodeRR MULr 
      "banr" -> OpCodeRR BANr 
      "borr" -> OpCodeRR BORr 
      "setr" -> OpCodeRR SETr 
      
      "gtri" -> OpCodeRI GTri 
      "eqri" -> OpCodeRI EQri 
      "addi" -> OpCodeRI ADDi 
      "muli" -> OpCodeRI MULi 
      "bani" -> OpCodeRI BANi 
      "bori" -> OpCodeRI BORi 

  [a, b, c] <- decimal `sepBy` " "

  pure $ eval ins a b (reg c)

parseProgram :: Parser (Int, [_])
parseProgram = do
  ip <- parseIP
  "\n"
  instrs <- parseInstr `sepBy` "\n"

  pure (ip, instrs)

runInstrs def (reg -> regIp) (Vector.fromList -> instrs) = go 0 def
  where
    go pc m = case traceShow m $ instrs Vector.!? pc of
      Nothing -> m
      Just i -> 
        let m' = runMachine i (Map.insert regIp pc m)
            pc' = fromMaybe (panic "dafuck") (Map.lookup regIp m')
        in go (pc' + 1) m'

-- * Generics

-- * FIRST problem
day :: _ -> Int
day = fromMaybe (panic "I'm sure") . Map.lookup R0 . uncurry (runInstrs defaultMachine)

-- * SECOND problem
day' :: _ -> Int
day' = fromMaybe (panic "I'm sure") . Map.lookup R0 . uncurry (runInstrs (Map.insert R0 1 defaultMachine))

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
