module All where

import Utils

import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
-- import Day7 -- not fixed
import Day8
import Day9
import Day10

test = hspec $ mapM_ (\(name, s) -> describe name s) $(thisModuleName)
