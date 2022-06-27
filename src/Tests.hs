import Collatz
  ( collatzECF,
    collatzECFtoN,
    collatzECFtoRECF,
    collatzRECF,
    collatzRECFtoN,
    collatzReducedSequence,
    collatzSequence,
  )
import Control.Exception (assert)
import Iterative (iterativePathExtension, iterativePrefix)
import PIPTree
  ( piptreeGetRootDirections,
    piptreePrefixFindByNumber,
    piptreePrefixFindByPath,
  )
import Prefix (prefixAdd, prefixFind, prefixID, prefixIterate)
import RIPTree (riptreeNextInPath, riptreePrefixFindByNumber, riptreePrefixFindByPath)
import System.Environment (getArgs)
import Utils (bton, isPow2, nextPow2, ntob, ntop, pton)

-- | Test functions from Utils.hs
testsUtils :: Int -> IO ()
testsUtils n = do
  putStrLn "\n--- Utils Tests ---"

  putStr "NTOP: "
  print $ map (\b -> if b then '1' else '0') (ntop n)

  putStr "PTON: "
  print (pton (ntop n))

  putStr "NTOB: "
  print $ map (\b -> if b then '1' else '0') (ntob n)

  putStr "BTON: "
  print (bton (ntob n))

  putStr "ISPOW2: "
  print (isPow2 n)

  putStr "NEXTPOW2: "
  print (nextPow2 n)

-- | Test functions from Collatz.hs
testsCollatz :: Int -> IO ()
testsCollatz n = do
  putStrLn "\n--- Collatz Tests ---"

  putStr "Sequence: "
  print (collatzSequence n)

  putStr "Reduced Sequence: "
  print (collatzReducedSequence n)

  putStr "ECF: "
  print (collatzECF n)

  putStr "RECF: "
  print (collatzRECF n)

  putStr (assert (n == collatzECFtoN (collatzECF n)) "")
  putStr (assert (n == collatzRECFtoN (collatzRECF n)) "")
  putStr (assert (collatzRECF n == collatzECFtoRECF (collatzECF n)) "")

-- | Test functions from Prefix.hs
testsPrefix :: Int -> Int -> IO ()
testsPrefix n m = do
  putStrLn "\n--- Prefix Tests ---"

  putStr "ECF N: "
  print (collatzECF n)

  putStr "ECF M: "
  print (collatzECF m)

  let pf = prefixFind n m
  putStr "Prefix N M: "
  print pf

  putStr "Prefix ID: "
  print (prefixID pf)

  putStr "Prefix Iterate N: "
  print (prefixIterate n pf)

  putStr (assert ([1, 2, 7, 8, 9] == prefixAdd [1, 2, 3] [4, 5, 6]) "")

  if not (null pf) then putStr (assert (odd (prefixIterate n pf)) "") else pure () -- pure() is NOP
  putStr (assert (1 == prefixIterate n (collatzECF n)) "")

-- | Test functions from RIPTree.hs
testsRIPTree :: Int -> IO ()
testsRIPTree n = do
  putStrLn "\n--- RIPTree Tests ---"

  putStr "ECF N: "
  print (collatzECF n)

  putStr "Path of N: "
  let p = ntop n :: [Bool]
  print $ map (\b -> if b then '1' else '0') p

  putStr "Next number M at path of N: "
  let m = riptreeNextInPath n p :: Int
  print m

  putStr "ECF M: "
  print (collatzECF m)

  putStr "RIPTree Prefix N: "
  let pf = riptreePrefixFindByNumber n :: [Int]
  print pf

  putStr "Prefix Iteration of N: "
  let pf_iter = prefixIterate n pf :: Int
  print pf_iter

  putStr (assert (odd pf_iter) "")

-- | Test functions from RIPTree.hs
testsPIPTree :: Int -> IO ()
testsPIPTree n = do
  putStrLn "\n--- PIPTree Tests ---"

  putStr "ECF N: "
  print (collatzECF n)

  putStr "PIPTree Prefix N: "
  let pf = piptreePrefixFindByNumber n :: [Int]
  print pf

  putStr "Prefix Iteration of N: "
  let pf_iter = prefixIterate n pf :: Int
  print pf_iter

  putStr (assert (pf == riptreePrefixFindByNumber n) "")
  putStr (assert (odd pf_iter) "")
  -- example case for piptreeGetRootDirections
  putStr (assert ([True, True, False] == piptreeGetRootDirections (ntob 14)) "")

-- | Test iterative methods
testIterative :: Int -> IO ()
testIterative n = do
  putStrLn "\n--- Iterative Tests ---"

  let ecf = collatzECF n

  let ecf_riptree_pf = iterativePrefix n riptreePrefixFindByNumber
  putStr (assert (ecf_riptree_pf == ecf) "")

  let ecf_piptree_pf = iterativePrefix n piptreePrefixFindByNumber
  putStr (assert (ecf_piptree_pf == ecf) "")

  let ecf_riptree_path = iterativePathExtension n riptreePrefixFindByPath
  putStr (assert (ecf_riptree_path == ecf) "")

  let ecf_piptree_path = iterativePathExtension n piptreePrefixFindByPath
  putStr (assert (ecf_piptree_path == ecf) "")

  putStr "ECF N: "
  print ecf

-- | Driver function for tests
main :: IO ()
main =
  do
    -- get command-line arguments
    args <- getArgs
    -- first number (n)
    let n =
          if null args
            then 5 -- default value for n
            else (read (head args) :: Int)
    -- second number (m)
    let m =
          if null args || null (tail args)
            then 13 -- default value for m
            else (read (head (tail args)) :: Int)

    testsUtils n
    testsCollatz n
    testsPrefix n m
    testsRIPTree n
    testsPIPTree n
    testIterative n

    putStrLn "\nAll tests pass baby :)"