module DayX where

import Utils

fileContent :: Text
fileContent = $(getFile)

-- * Generics


-- * FIRST problem
day :: _ -> Int
day = undefined

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
