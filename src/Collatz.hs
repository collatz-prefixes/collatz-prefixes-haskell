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

-- | Find the RECF (Reduced ECF) of a number.
collatzRECF :: Int -> [Int]
collatzRECF n = _collatzRECF n 0
  where
    _collatzRECF :: Int -> Int -> [Int]
    _collatzRECF n t
      | n > 1 && even n = _collatzRECF (consumeTwos n) (numTwos n)
      | n > 1 && odd n = t : _collatzRECF (3 * n + 1) 0
      | otherwise = [t]

-- | Compute a number from it's RECF.
collatzRECFtoN :: [Int] -> Int
collatzRECFtoN ecf = _collatzRECFtoN (tail (reverse ecf)) (2 ^ last ecf)
  where
    _collatzRECFtoN :: [Int] -> Int -> Int
    _collatzRECFtoN [] n = n
    _collatzRECFtoN ecf n =
      _collatzRECFtoN
        (tail ecf)
        (div (n - 1) 3 * (2 ^ head ecf))

-- | Compute a number from it's ECF. Converts ECF to RECF, and then finds the number.
collatzECFtoN :: [Int] -> Int
collatzECFtoN ecf = _collatzECFtoN (reverse (tail ecf)) 1
where
  _collatzECFtoN  :: [Int] -> Int -> Int
  _collatzECFtoN [] ans = ans
  _collatzECFtoN (ecf n = 
    -- todo todo todo

-- | Convert the ECF to RECF
collatzECFtoRECF :: [Int] -> [Int]
collatzECFtoRECF ecf = head ecf : _collatzECFtoRECF (tail ecf) (head ecf)
  where
    _collatzECFtoRECF :: [Int] -> Int -> [Int]
    _collatzECFtoRECF [] last = []
    _collatzECFtoRECF ecf last = (head ecf - last) : _collatzECFtoRECF (tail ecf) (head ecf)

-- TODO: ICF implement