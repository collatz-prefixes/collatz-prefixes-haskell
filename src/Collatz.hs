module Collatz where

import Utils (consumeTwos, numTwos)

-- | Collatz length is the number of iterations it takes to reach n to 1
collatzLength :: Int -> Int
collatzLength n
  | n > 1 && even n = 1 + collatzLength (div n 2)
  | n > 1 && odd n = 1 + collatzLength (n * 3 + 1)
  | otherwise = 0

-- | Collatz Sequence is the array of numbers seen during iterations until 1 is reached
collatzSequence :: Int -> [Int]
collatzSequence n
  | n > 1 && even n = n : collatzSequence (div n 2)
  | n > 1 && odd n = n : collatzSequence (n * 3 + 1)
  | otherwise = [1]

-- | A reduced sequence is a sequence but even numbers (except the starting number) are omitted.
collatzReducedSequence :: Int -> [Int]
collatzReducedSequence n
  | n > 1 && even n = n : collatzReducedSequence (consumeTwos n)
  | n > 1 && odd n = n : collatzReducedSequence (consumeTwos (3 * n + 1))
  | otherwise = [1]

-- | Find ECF (Exponential Canonical Form) of a number.
collatzECF :: Int -> [Int]
collatzECF n = _collatzECF n 0
  where
    _collatzECF :: Int -> Int -> [Int]
    _collatzECF n t
      | n > 1 && even n = _collatzECF (consumeTwos n) (t + numTwos n)
      | n > 1 && odd n = t : _collatzECF (3 * n + 1) t
      | otherwise = [t]

-- | Compute a number from it's ECF.
collatzECFtoN :: [Int] -> Int
collatzECFtoN ecf = _collatzECFtoN (reverse ecf) 1
  where
    _collatzECFtoN :: [Int] -> Int -> Int
    _collatzECFtoN [] _ = 1
    _collatzECFtoN [last] ans = ans * 2 ^ last
    _collatzECFtoN (last : ecf) ans =
      _collatzECFtoN
        ecf
        (div (ans * 2 ^ (last - head ecf) - 1) 3)
