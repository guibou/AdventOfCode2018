module Day14 where

import Utils

import qualified Data.Vector.Mutable as VectorM
import qualified Data.Vector as Vector

fileContent :: Int
fileContent = 290431

-- start 17h25 -- 18h19??? !!! --> 18h32

-- * Generics
data Brew = Brew !Int !Int !Int (VectorM.IOVector Int)

startBrew :: Int -> IO Brew
startBrew i = do
  v <- VectorM.replicate (i * 2) 0
  VectorM.write v 0 3
  VectorM.write v 1 7

  pure $ Brew 0 1 2 v

nextBrew :: Brew -> IO Brew
nextBrew (Brew idA idB len v) = do
  rA <- VectorM.read v idA
  rB <- VectorM.read v idB

  let foo = rA + rB

  if foo >= 10
    then do
    let (a, b) = divMod foo 10
    VectorM.write v len a
    VectorM.write v (len + 1) b

    let len' = len + 2

    pure $ Brew ((idA + 1 + rA) `mod` len') ((idB + 1 + rB) `mod` len') len' v

    else do
    VectorM.write v len foo

    let len' = len + 1

    pure $ Brew ((idA + 1 + rA) `mod` len') ((idB + 1 + rB) `mod` len') len' v

score10 :: Int -> Brew -> IO [Char]
score10 i (Brew _ _ _ v) = do
  v' <- Vector.unsafeFreeze v

  pure $ concatMap show $ Vector.toList $ Vector.take 10 (Vector.drop i v')

-- * FIRST problem
day :: Int -> IO [Char]
day n = do
  score10 n =<< applyNM (n + 10) nextBrew =<< startBrew (n + 10)

day' :: Int -> [Int] -> IO Int
day' n searchVal = do
  Brew _ _ _ res <- applyNM (n + 10) nextBrew =<< startBrew (n + 10)

  v <- Vector.unsafeFreeze res

  pure $ go 0 v
  where 
    needle = Vector.fromList searchVal
    len = length searchVal

    go i v = let slice = Vector.slice i len v
             in if slice == needle
                then i
                else go (i + 1) v

display :: Brew -> IO ()
display (Brew a b l v) = do
  v' <- Vector.unsafeFreeze v
  let val = Vector.take l v'
  print (a, b, val)

applyNM :: Int -> (t -> IO t) -> t -> IO t
applyNM 0 _op v = pure v
applyNM n op v = applyNM (n - 1) op =<< op v
 
-- * SECOND problem
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
