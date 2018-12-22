module Day16 where

-- start : 21h38 -> 22h41 -> 23h17

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import qualified Control.Monad.State as S
import qualified Data.Map as Map
import Utils hiding (R1)
import Data.List (delete)

parseList :: Parser [Int]
parseList = do
  "["
  l <- decimal `sepBy` ", "
  "]"
  pure l

parseExample :: Parser ([Int], [Int], [Int])
parseExample = do
  "Before: "
  l0 <- parseList
  "\n"
  op <- decimal `sepBy` " "
  "\n"
  "After:  "
  l1 <- parseList
  "\n"

  pure (l0, op, l1)

parseExamples = Text.Megaparsec.some (parseExample <* "\n")

parseFile = do
  exs <- parseExamples
  "\n\n"
  instr <- ((decimal :: Parser Int) `sepBy` " ") `sepBy` "\n"

  pure (exs, instr)
  

fileContent = unsafeParse parseFile $(getFile)

data Register = R0 | R1 | R2 | R3
  deriving (Show, Ord, Eq, Enum, Bounded)

newtype Machine t = Machine (S.State (Map Register Int) t)
  deriving newtype (Functor, Applicative, Monad)

read :: Register -> Machine Int
read reg = Machine (gets (fromMaybe undefined . Map.lookup reg))

write :: Register -> Int -> Machine ()
write reg i = Machine (modify (Map.insert reg i))

runMachine :: Machine () -> Map Register Int -> Map Register Int
runMachine (Machine s) m = let ((), res) = runState s m in res

defaultMachine :: Map Register Int
defaultMachine = mFromList [0, 0, 0, 0]

mFromList :: [Int] -> Map Register Int
mFromList l = Map.fromList (zip [minBound .. maxBound] l)

class RegisterValue t where
  value :: t -> Machine Int

instance RegisterValue Register where
  value reg = read reg

instance RegisterValue Int where
  value i = pure i

-- * Opcodes
op :: RegisterValue roi => (Int -> Int -> Int) -> Register -> roi -> Register -> Machine ()
op op inA inB outC = do
  a <- read inA
  b <- value inB
  write outC (op a b)

addr = op @Register (+)
addi = op @Int (+)

mulr = op @Register (*)
muli = op @Int (*)

banr = op @Register (.&.)
bani = op @Int (.&.)
  
borr = op @Register (.|.)
bori = op @Int (.|.)

setr :: RegisterValue roi => Register -> roi -> Register -> Machine ()
setr regA _regB regC = read regA >>= write regC

seti :: RegisterValue roi => Int -> roi -> Register -> Machine ()
seti i _regB regC = write regC i

--

cmp :: (RegisterValue roi0, RegisterValue roi1) => (Int -> Int -> Bool) -> roi0 -> roi1 -> Register -> Machine ()
cmp f inA inB outC = do
  a <- value inA
  b <- value inB

  write outC $ if f a b then 1 else 0

gtir = cmp @Int @Register (>)
gtri = cmp @Register @Int (>)
gtrr = cmp @Register @Register (>)

eqir = cmp @Int @Register (==)
eqri = cmp @Register @Int (==)
eqrr = cmp @Register @Register (==)


data OpCodes =
  GTir | EQir | SETi |
  GTrr | EQrr | ADDr | MULr | BANr | BORr | SETr |
  GTri | EQri | ADDi | MULi | BANi | BORi
  deriving (Show, Eq, Ord)

irs = [GTir, EQir, SETi]
rrs = [GTrr, EQrr, ADDr, MULr, BANr, BORr, SETr]
ris = [GTri, EQri, ADDi, MULi, BANi, BORi]

matchingOpCode :: (_, _, _) -> ([Int], [Int], [Int]) -> _
matchingOpCode (irs, rrs, ris) (before, [_opCode, a, b, c], after) =
  let
    beforeS = mFromList before
    afterS = mFromList after
  in ( filter (\op -> testOp (toOpIR op a (reg b) (reg c)) beforeS afterS) irs
     , filter (\op -> testOp (toOpRR op (reg a) (reg b) (reg c)) beforeS afterS) rrs
     , filter (\op -> testOp (toOpRI op (reg a) b (reg c)) beforeS afterS) ris
     )

isIR = \case
  GTir -> True
  EQir -> True
  SETi -> True
  _ -> False

isRR = \case
  GTrr -> True
  EQrr -> True
  ADDr -> True
  MULr -> True
  BANr -> True
  BORr -> True
  SETr -> True
  _ -> False

isRI = \case
  GTri -> True
  EQri -> True
  ADDi -> True
  MULi -> True
  BANi -> True
  BORi -> True
  _ -> False

toOpIR = \case
  GTir -> gtir
  EQir -> eqir
  SETi -> seti

toOpRR = \case
  GTrr -> gtrr
  EQrr -> eqrr
  ADDr -> addr
  MULr -> mulr
  BANr -> banr
  BORr -> borr
  SETr -> setr

toOpRI = \case
  GTri -> gtri
  EQri -> eqri
  ADDi -> addi
  MULi -> muli
  BANi -> bani
  BORi -> bori
  
test :: ([Int], [Int], [Int]) -> _
test sample = 
  let
    (mIR, mRR, mRI) = matchingOpCode (irs, rrs, ris) sample
  in length mIR + length mRR + length mRI

testOp :: Machine () -> Map Register Int -> Map Register Int -> Bool
testOp op before after =
  let after' = runMachine op before
  in after' == after

reg 0 = R0
reg 1 = R1
reg 2 = R2
reg 3 = R3

countSamples samples = length (filter (\s -> test s >= 3) samples)


-- part2

getSampleOpCode (_, opCode:_, _) = opCode

associateOpcodes :: [_] -> Map Int OpCodes
associateOpcodes samples = go Map.empty samples (irs, rrs, ris)
  where
    go m _ ([], [], []) = m
    go m [] tests = go m samples tests
    go m (sample:xs) tests@(irs, rrs, ris) = let
      (mIR, mRR, mRI) = matchingOpCode tests sample
      matchings = mIR ++ mRR ++ mRI
      in case matchings of
           [x] -> go (Map.insert (getSampleOpCode sample) x m) xs (x `delete` irs, x `delete` rrs, x `delete` ris)
           _ -> go m xs tests

-- * Generics
applyProgram :: (_, _) -> _
applyProgram (samples, program) = let
  opcodes = associateOpcodes samples
  res = runMachine (makeProgram opcodes program) defaultMachine
  in Map.lookup R0 res

makeProgram :: Map Int OpCodes -> [[Int]] -> Machine ()
makeProgram assoc l = go l
  where
    go [] = pure ()
    go (x:xs) = opCodeToInstr assoc x >> go xs

opCodeToInstr assoc [opCode, a, b, c] = let
  fn = fromMaybe undefined (Map.lookup opCode assoc)
  in eval fn a b (reg c)

eval opcode a b
  | isRR opcode = toOpRR opcode (reg a) (reg b) 
  | isRI opcode = toOpRI opcode (reg a) b
  | isIR opcode = toOpIR opcode a (reg b) 

-- * FIRST problem
day :: _ -> Int
day = countSamples

-- * SECOND problem
day' :: _ -> Int
day' = undefined

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
