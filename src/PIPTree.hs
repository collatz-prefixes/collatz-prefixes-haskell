module PIPTree where

import Prefix (prefixIterate)
import Utils (bton, isPow2, ntop, numTwos, pton)

-- | Finds the nature of a node at the given path, with prefix pf and the root prefix rpf
piptreeFindNature :: [Bool] -> [Int] -> [Int] -> Bool
piptreeFindNature p pf rpf =
  even
    ( prefixIterate
        (pton p) -- number at the path
        (pf ++ map (+ 1) rpf) -- [p1, p2, ..., pk, rpf + 1]
    )

-- | Finds the direction from root to a specified path
piptreeGetRootDirections :: [Bool] -> [Bool]
piptreeGetRootDirections p = reverse (_piptreeGetRootDirections (bton p))
  where
    _piptreeGetRootDirections :: Int -> [Bool]
    _piptreeGetRootDirections n
      | n > 1 && even n = False : _piptreeGetRootDirections (div n 2)
      | n > 1 && odd n = True : _piptreeGetRootDirections (div (n - 1) 2)
      | otherwise = []

-- Finds the prefix of a given number via PIPTree
piptreePrefixFindByNumber :: Int -> [Int]
piptreePrefixFindByNumber n = piptreePrefixFindByPath (ntop n)

-- Finds the prefix of a number at the given path via PIPTree
piptreePrefixFindByPath :: [Bool] -> [Int]
piptreePrefixFindByPath p
  | isPow2 (pton p) = [numTwos (pton p)]
  | otherwise =
    _piptreePrefixFindByPath
      (piptreeGetRootDirections p) -- direction
      (2 ^ (length p - 1)) -- current number (which is root)
      (replicate (length p - 1) False ++ [True]) -- current path [00..01] (which is root)
      [length p - 1] -- current prefix (which is root)
  where
    _piptreePrefixFindByPath :: [Bool] -> Int -> [Bool] -> [Int] -> [Int]
    _piptreePrefixFindByPath [] n p pf = pf
    _piptreePrefixFindByPath dirs n p pf =
      if head dirs == False -- left
        then
          _piptreePrefixFindByPath
            (tail dirs) -- rest of the directions
            (div n 2 + r) -- next number
            (tail p ++ [False]) -- next path
            (map (subtract 1) pf ++ if piptreeFindNature p pf rpf == True then rpf else []) -- next prefix
        else
          _piptreePrefixFindByPath
            (tail dirs) -- rest of the directions
            (div n 2) -- next number
            (tail p ++ [True]) -- next path
            (map (subtract 1) pf ++ if piptreeFindNature p pf rpf == False then rpf else []) -- next prefix
      where
        r = 2 ^ (length p - 1) -- root number
        rpf = [length p - 1] -- root prefix
