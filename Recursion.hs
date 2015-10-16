module Recursion where
import Debug.Trace

(//) :: Int -> Int -> Int
x // y = x `div` y
(%) :: Int -> Int -> Int
x % y = x `mod` y

-- Precondition on all integers: they're all non-negative.

isPrime :: Int -> Bool
-- Determines the primality of a given number
isPrime x
  | x == 2          = True
  | x < 2 || even x = False
  | otherwise       = isPrime' 3
  where
    isPrime' :: Int -> Bool
    isPrime' y
      | y  > maxNum     = True
      | x % y == 0  = False
      | otherwise       = isPrime' (y + 2)
      where
		    maxNum = ceiling(sqrt(fromIntegral(x)))


nextPrime :: Int -> Int
-- Finds the next prime number after a given integer
nextPrime x
  | x <= 1  = 2   -- Allows the +2 optimisation on line +7 without any issue
  | odd x   = nextPrime' (x + 2)
  | even x  = nextPrime' (x + 1)
  where
    nextPrime' :: Int -> Int
    nextPrime' y
      | isPrime y = y
      | otherwise = nextPrime' (y + 2)


modPow :: Int -> Int -> Int -> Int
-- Pre: 1 <= m <= sqrt(maxint)
-- Calculates x^y mod n while minimising y to 1 for all exponents so that
--   no integer overflow occurs for x <= maxint.
-- Calculation context:
--   For even y, x^y mod n = (x^(y/2) mod n)^2 mod n
--   For odd  y, x^y mod n = (x mod n)(x^(y-1) mod n) mod n
modPow x y n
  | y <= 1  = (x^y) % n
  | odd y   = ((x % n) * (modPow x (y - 1) n)) % n
  | even y  = (((modPow x (y // 2) n) % n) ^ 2) % n


isCarmichael :: Int -> Bool
-- Returns whether integer x is a carmichael number or not, by checking
--   if every lower integer passes the Fermat test (modPow y x x == y)
isCarmichael x
  | isPrime x = False
  | otherwise = isCarmichael' 2
  where
    isCarmichael' :: Int -> Bool
    isCarmichael' y 
      | y == x    = True
      | otherwise = (modPow y x x == y) && isCarmichael' (y + 1)

primeFactors :: Int -> [ Int ]
-- Pre: x >= 1
-- Returns a list of the prime factors for x
-- Interesting list hack (see primeCandidates) saves about 0.4 seconds per
--   large operation (>10m) as compared to methods e.x. "y1+1+y%2" or guard
--   for y == 2
-- This difference is significantly noticeable for smith numbers so kept
--   despite readability issue
primeFactors x
  | isPrime x = [x]
  | otherwise = primeFactors' x primeCandidates
  where
    primeCandidates = 2:[3,5..]
    primeFactors' :: Int -> [ Int ] -> [ Int ]
    primeFactors' x' all@(y:candidates)
      | x' == 1         = []
      | x' % y == 0 = y : primeFactors' (x' // y) all
      | otherwise       = primeFactors' x' candidates


splitDigits :: Int -> [Int]
-- Splits x from a (multi)digit number into a list of it's constituent digits
splitDigits x
  | x < 10  = [x]
  | otherwise = splitDigits (x // 10) ++ [x % 10]


sumDigits :: Int -> Int
-- Sums the digits of x using the splitDigits function
sumDigits x
  = sum $ splitDigits x


sumAllDigits :: [ Int ] -> Int
-- Applies sumDigits to a list
sumAllDigits x
  = sum $ map sumDigits x

isSmith :: Int -> Bool
-- Calculates and returns whether a given number x is a smith number or not.
isSmith x
  = sumDigits x == (sumAllDigits $ primeFactors x)

nextSmithNumber :: Int -> Int
-- Calculates the next smith number from x.
-- chk: Variable representing one above x to check for smith-ness.
--   Saves writing (and potentially calculating) x + 1 repeatedly
nextSmithNumber x
  | isSmith (chk) && (not $ isPrime (chk)) = chk
  | otherwise = nextSmithNumber (chk)
  where
    chk = x + 1
