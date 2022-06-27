module Utils where

import Data.Bits ((.&.))

-- | Consumes twos in a number, so the end result is always an odd number
consumeTwos :: Int -> Int
consumeTwos n
  | even n = consumeTwos (div n 2)
  | otherwise = n

-- | Returns the number of twos as a factor in a number
numTwos :: Int -> Int
numTwos n
  | even n = 1 + numTwos (div n 2)
  | otherwise = 0

-- | Obtain the binary representation of a number
ntob :: Int -> [Bool]
ntob 0 = [False]
ntob n = _ntob n []
  where
    _ntob :: Int -> [Bool] -> [Bool]
    _ntob 0 b = b
    _ntob n b = _ntob (div n 2) (odd n : b)

-- | Find the number from its binary representation
bton :: [Bool] -> Int
bton b = _bton b 0
  where
    _bton :: [Bool] -> Int -> Int
    _bton [] n = n
    _bton b n = _bton (tail b) (2 * n + if head b then 1 else 0)

-- | Obtain the shortest path to the given number
ntop :: Int -> [Bool]
ntop n = map not (reverse (ntob (n - 1)))

-- | Finds the first number that resides at the given path
pton :: [Bool] -> Int
pton p = bton (reverse (map not p)) + 1

-- | Find if a given number is a power of two
isPow2 :: Int -> Bool
isPow2 n = n /= 0 && (n .&. (n - 1) == 0)

-- | Find the smallest power of two greater than or equal to the given number
nextPow2 :: Int -> Int
nextPow2 n = if isPow2 n then n else _nextPow2 1
  where
    _nextPow2 :: Int -> Int
    _nextPow2 p
      | p < n = _nextPow2 (2 * p)
      | otherwise = p