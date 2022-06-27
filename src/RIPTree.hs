module RIPTree where

import Prefix (prefixFind)
import Utils (isPow2, ntop, numTwos, pton)

-- | Finds the next number that resides at the path of n (or a given path)
riptreeNextInPath :: Int -> [Bool] -> Int
riptreeNextInPath n p = n + 2 ^ length p

-- | Finds the prefix of a given number by looking at the RIPTree
riptreePrefixFindByNumber :: Int -> [Int]
riptreePrefixFindByNumber n = riptreePrefixFindByPath (ntop n)

-- | Finds the prefix of a number at the given path by looking at the RIPTree
riptreePrefixFindByPath :: [Bool] -> [Int]
riptreePrefixFindByPath p = do
  let n = pton p
  if isPow2 n
    then [numTwos n]
    else prefixFind n (riptreeNextInPath n p)
