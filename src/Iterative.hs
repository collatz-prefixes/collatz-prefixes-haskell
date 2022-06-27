module Iterative where

import Prefix (prefixAdd, prefixIterate)
import RIPTree (riptreePrefixFindByNumber)
import Utils (ntop)

-- | Find the ECF of a number by extending it's path.
iterativePathExtension :: Int -> ([Bool] -> [Int]) -> [Int]
iterativePathExtension n prefixFinder = prefixFinder (_iterativePathExtension (ntop n))
  where
    _iterativePathExtension :: [Bool] -> [Bool]
    _iterativePathExtension p
      | prefixIterate n (prefixFinder p) == 1 = p
      | otherwise = _iterativePathExtension (p ++ [True])

-- | Find the ECF of a number by adding it's prefixes
iterativePrefix :: Int -> (Int -> [Int]) -> [Int]
iterativePrefix n prefixFinder = _iterativePrefix n []
  where
    _iterativePrefix :: Int -> [Int] -> [Int]
    _iterativePrefix cur ans = do
      let pf = prefixFinder cur
      let anspf = prefixAdd ans pf
      let next = prefixIterate cur pf
      if next == 1
        then anspf
        else
          if not (null anspf)
            then _iterativePrefix (3 * next + 1) (anspf ++ [last anspf])
            else _iterativePrefix (3 * next + 1) anspf
