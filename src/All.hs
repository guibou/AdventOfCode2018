module All where

import Utils

test = hspec $ mapM_ (\(name, s) -> describe name s) $(thisModuleName)
