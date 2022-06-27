module Prefix where

import Collatz (collatzECF)

-- | Returns the prefix of two numbers.
prefixFind :: Int -> Int -> [Int]
prefixFind n m
  | n == m = collatzECF n
  | otherwise = _prefixFind n m 0
  where
    _prefixFind :: Int -> Int -> Int -> [Int]
    _prefixFind n m twos
      | even n && even m = _prefixFind (div n 2) (div m 2) (twos + 1)
      | odd n && odd m = twos : _prefixFind (3 * n + 1) (3 * m + 1) twos
      | otherwise = []

-- | Iterates a number through a prefix. If the prefix is equal to ECF of the number, the result is expected to be 1.
prefixIterate :: Int -> [Int] -> Int
prefixIterate n [] = n
prefixIterate n pf = _prefixIterate (div n (2 ^ head pf)) (tail pf) (head pf)
  where
    _prefixIterate :: Int -> [Int] -> Int -> Int
    _prefixIterate n [] last = n
    _prefixIterate n pf last =
      _prefixIterate
        (div (3 * n + 1) (2 ^ (head pf - last)))
        (tail pf)
        (head pf)

-- | Attach two prefixes together
prefixAdd :: [Int] -> [Int] -> [Int]
prefixAdd [] pf2 = pf2
prefixAdd pf1 pf2 =
  init pf1 ++ map (+ last pf1) pf2 -- pf1[:-1] + (pf2[:] + pf1[-1])

-- | Prefix ID is a binary interpretation of prefix
prefixID :: [Int] -> Int
prefixID [] = 0
prefixID pf = _prefixID pf 0
  where
    _prefixID :: [Int] -> Int -> Int
    _prefixID [] ans = ans
    _prefixID pf ans = _prefixID (tail pf) (ans + 2 ^ head pf)