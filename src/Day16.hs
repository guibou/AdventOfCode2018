{-# LANGUAGE DataKinds, KindSignatures, GADTs #-}
{-# OPTIONS -Wno-name-shadowing #-}
module Day16 where

-- start : 21h38 -> 22h41 -> 23h17

import Text.Megaparsec
import Text.Megaparsec.Char.Lexer

import qualified Control.Monad.State as S
import qualified Data.Map as Map
import Utils hiding (R1)
import Data.List (delete)

parseList :: Parser [Int]
parseList = "[" *> (decimal `sepBy` ", ") <* "]"

parseExample :: Parser ([Int], [Int], [Int])
parseExample = do
  _ <- "Before: "
  l0 <- parseList
  _ <- "\n"
  op <- decimal `sepBy` " "
  _ <- "\n"
  _ <- "After:  "
  l1 <- parseList
  _ <- "\n"

  pure (l0, op, l1)

parseExamples = Text.Megaparsec.some (parseExample <* "\n")

parseFile = do
  exs <- parseExamples
  _ <- "\n\n"
  instr <- ((decimal :: Parser Int) `sepBy` " ") `sepBy` "\n"

  pure (exs, instr)
  

fileContent = unsafeParse parseFile $(getFile)

data Register = R0 | R1 | R2 | R3 | R4 | R5 -- TODO: generic between day 19 and day 16
  deriving (Show, Ord, Eq, Generic, Enumerable)

newtype Machine t = Machine (S.State (Map Register Int) t)
  deriving newtype (Functor, Applicative, Monad)

read :: Register -> Machine Int
read reg = Machine (gets (fromMaybe (panic "I Know my machine has this register") . Map.lookup reg))

write :: Register -> Int -> Machine ()
write reg i = Machine (modify (Map.insert reg i))

runMachine :: Machine () -> Map Register Int -> Map Register Int
runMachine (Machine s) m = let ((), res) = runState s m in res

defaultMachine :: Map Register Int
defaultMachine = mFromList (repeat 0)

mFromList :: [Int] -> Map Register Int
mFromList l = Map.fromList (zip enumerated l)

class RegisterValue t where
  value :: t -> Machine Int

instance RegisterValue Register where
  value reg = read reg

instance RegisterValue Int where
  value i = pure i

-- * Opcodes
op :: (RegisterValue roiA, RegisterValue roiB) => (Int -> Int -> Int) -> roiA -> roiB -> Register -> Machine ()
op op inA inB outC = do
  a <- value inA
  b <- value inB
  write outC (op a b)

addr = op @Register @Register (+)
addi = op @Register @Int (+)

mulr = op @Register @Register (*)
muli = op @Register @Int (*)

banr = op @Register @Register (.&.)
bani = op @Register @Int (.&.)
  
borr = op @Register @Register (.|.)
bori = op @Register @Int (.|.)

setr :: RegisterValue roi => Register -> roi -> Register -> Machine ()
setr regA _regB regC = read regA >>= write regC

seti :: RegisterValue roi => Int -> roi -> Register -> Machine ()
seti i _regB regC = write regC i

--

cmp :: (RegisterValue roi0, RegisterValue roi1) => (Int -> Int -> Bool) -> roi0 -> roi1 -> Register -> Machine ()
cmp f = op (\a b -> if f a b then 1 else 0)

gtir = cmp @Int @Register (>)
gtri = cmp @Register @Int (>)
gtrr = cmp @Register @Register (>)

eqir = cmp @Int @Register (==)
eqri = cmp @Register @Int (==)
eqrr = cmp @Register @Register (==)

-- OPCodes
data OpCodeType = RR | RI | IR
  deriving (Show)

data OpCode
  = OpCodeIR OpCodeIR
  | OpCodeRR OpCodeRR
  | OpCodeRI OpCodeRI
  deriving (Show, Eq, Ord, Generic, Enumerable)

data OpCodeIR = GTir | EQir | SETi
  deriving (Show, Eq, Ord, Generic, Enumerable)

data OpCodeRR = GTrr | EQrr | ADDr | MULr | BANr | BORr | SETr
  deriving (Show, Eq, Ord, Generic, Enumerable)

data OpCodeRI = GTri | EQri | ADDi | MULi | BANi | BORi
  deriving (Show, Eq, Ord, Generic, Enumerable)

matchingOpCode :: _ -> ([Int], [Int], [Int]) -> _
matchingOpCode ops (before, [_opCode, a, b, c], after) =
  let
    beforeS = mFromList before
    afterS = mFromList after
  in filter (\op -> testOp (eval op a b (reg c)) beforeS afterS) ops

toOpIR :: OpCodeIR -> _
toOpIR = \case
  GTir -> gtir
  EQir -> eqir
  SETi -> seti

toOpRR :: OpCodeRR -> _
toOpRR = \case
  GTrr -> gtrr
  EQrr -> eqrr
  ADDr -> addr
  MULr -> mulr
  BANr -> banr
  BORr -> borr
  SETr -> setr

toOpRI :: OpCodeRI -> _
toOpRI = \case
  GTri -> gtri
  EQri -> eqri
  ADDi -> addi
  MULi -> muli
  BANi -> bani
  BORi -> bori

testSample :: ([Int], [Int], [Int]) -> _
testSample sample = length $ matchingOpCode enumerated sample

testOp :: Machine () -> Map Register Int -> Map Register Int -> Bool
testOp op before after =
  let after' = runMachine op before
  in after' == after

reg 0 = R0
reg 1 = R1
reg 2 = R2
reg 3 = R3
reg 4 = R4
reg 5 = R5
reg _ = panic "Wrong register number"

countSamples samples = length (filter (\s -> testSample s >= 3) samples)


-- part2

getSampleOpCode (_, opCode:_, _) = opCode

associateOpcodes :: [_] -> Map Int OpCode
associateOpcodes samples = go Map.empty samples enumerated
  where
    go m _ [] = m
    go m [] tests = go m samples tests
    go m (sample:xs) tests = let
      matchings = matchingOpCode tests sample
      in case matchings of
           [x] -> go (Map.insert (getSampleOpCode sample) x m) xs (x `delete` tests)
           _ -> go m xs tests

-- * Generics
applyProgram :: (_, _) -> _
applyProgram (samples, program) = let
  opcodes = associateOpcodes samples
  res = runMachine (makeProgram opcodes program) defaultMachine
  in fromMaybe (panic "I know there is a register 0") $ Map.lookup R0 res

makeProgram :: Map Int OpCode -> [[Int]] -> Machine ()
makeProgram assoc l = go l
  where
    go [] = pure ()
    go (x:xs) = opCodeToInstr assoc x >> go xs

opCodeToInstr assoc [opCode, a, b, c] = let
  fn = fromMaybe (panic "...") (Map.lookup opCode assoc)
  in eval fn a b (reg c)

eval opcode a b = case opcode of
  OpCodeRR rr -> toOpRR rr (reg a) (reg b) 
  OpCodeRI ri -> toOpRI ri (reg a) b
  OpCodeIR ir -> toOpIR ir a (reg b) 

-- * FIRST problem
day :: _ -> Int
day = countSamples

-- * SECOND problem
day' :: _ -> _
day' = applyProgram

-- * Tests

test :: Spec
test = do
 describe "works" $ do
   it "on first star" $ do
     day (fst fileContent) `shouldBe` 531
   it "on second star" $ do
     day' fileContent `shouldBe` 649
