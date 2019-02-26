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
-- import Day11
import Day12
import Day13
-- import Day14 -- I don't know the parameter of the solution ;)
import Day15
-- import Day16 -- It works, but with lot of changes for future code (ASM!)
import Day17
import Day18
-- import Day19 -- No automated tests (ASM!)
import Day20
-- import Day21 -- ASM!
import Day22
-- import Day23 -- Timer?
import Day24
import Day25

test = hspec $ mapM_ (\(name, s) -> describe name s) $(thisModuleName)
