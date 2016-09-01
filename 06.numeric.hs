module Numeric
where

import System.Random

--
-- First approach to primality checking -------------------------------
--
-- Find the list of non-trivial factors of n (that is, omit 1 and n):

factors :: Integer -> [Integer]
factors n = [f | f <- [2 .. n-1], n `mod` f == 0]

-- A prime p is an integer which has no non-trivial factors:

primes :: [Integer]
primes
  = [i | i <- [2..], null (factors i)]

-- One way of testing for primality:

isElemOf :: Ord a => a -> [a] -> Bool
x `isElemOf` []
  = False
x `isElemOf` (y:ys)
  | x <= y    = x == y
  | otherwise = x `isElemOf` ys

isPrime1 :: Integer -> Bool
isPrime1 n
  | n < 1     = error "not a positive integer"
  | otherwise = n `isElemOf` primes

--
-- Second approach to primality checking ------------------------------
--
-- Alternatively, to generate primes, use the sieve of Eratosthenes:

sieve :: [Integer] -> [Integer]
sieve (x:xs) = x : sieve [y | y <- xs, y `mod` x > 0]

isPrime2 :: Integer -> Bool
isPrime2 n
  | n < 1     = error "not a positive integer"
  | otherwise = n `isElemOf` sieve [2..]

--
-- Third approach to primality checking, after Doets and Van Eijk -----
--

isPrime3 :: Integer -> Bool
isPrime3 n 
  | n < 1     = error "not a positive integer"
  | n == 1    = False
  | otherwise = ldp n == n 
    where
      ldp    = ldpf primes
      ldpf (p:ps) m | rem m p == 0 = p
                    | p^2 > m      = m
                    | otherwise    = ldpf ps m
      primes = 2 : filter isPrime3 [3..]


-- Find primes that are Mersenne primes, and primes that are not:

mersenne 
  = [(p,2^p - 1) | p <- primes, isPrime3 (2^p-1)]

notMersenne 
  = [(p,2^p - 1) | p <- primes, not (isPrime3 (2^p-1))]

-- Find prime pairs, that is, (p,p+2) where both p and p+2 are prime:

primePairs :: [(Integer,Integer)] 
primePairs 
  = pairs primes 
    where 
      pairs (x:y:xys) 
        | x + 2 == y = (x,y): pairs (y:xys)
        | otherwise  = pairs (y:xys)

-- Find prime triples (p,p+2,p+4) where all three are prime:

primeTriples :: [(Integer,Integer,Integer)]
primeTriples 
  = triples primes 
    where 
      triples (x:y:z:xyzs) 
        | x + 2 == y && y + 2 == z = (x,y,z) : triples (y:z:xyzs)
        | otherwise                = triples (y:z:xyzs)

--
-- Fourth approach to primality checking ------------------------------
--
-- Resort to fast probabilistic checking:

-- Calculate b^e mod m:

expmod :: Integer -> Integer -> Integer -> Integer
expmod b e m
  | e == 0    = 1
  | even e    = square (expmod b (e `div` 2) m) `mod` m
  | otherwise = (b * (expmod b (e-1) m)) `mod` m
    where
      square n = n*n

-- The number of times we want to repeat Fermat's test for a
-- given candidate prime:

fermatRepeats :: Int
fermatRepeats = 100

-- The test itself.  The type is an input/output action
-- because randomRs is monadic.

fermat :: Integer -> IO ()
fermat n
  = do
      randoms <- randomList fermatRepeats 2 (n-1)
      let looksLikePrime = and [a == expmod a n n | a <- randoms]
      let qual = if looksLikePrime then "" else "not "
      putStr (show n ++ " is " ++ qual ++ "prime\n")

-- Generate n random integers in the range lower .. upper:

randomList :: Int -> Integer -> Integer -> IO [Integer]
randomList n lower upper
  = do
      gen <- getStdGen
      let rs = randomRs (lower, upper) gen
      return (take n rs)

