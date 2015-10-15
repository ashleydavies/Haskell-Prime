module Recursion where

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
      | x `mod` y == 0  = False
      | otherwise       = isPrime' (y + 2)
      where
        -- sqrt x is the highest number we need to check. + Some necessary
        --   casts
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
  | y <= 1  = x^y `mod` n
  | odd y   = ((x `mod` n) * (modPow x (y - 1) n)) `mod` n
  | even y  = (((modPow x (y `div` 2) n) `mod` n) ^ 2) `mod` n


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
-- nextTest: Small optimisation. Checks (y + 1) next for even numbers (2),
--   checks y + 2 next for odd numbers (3, 5, ..). Avoids redundant odd checks
primeFactors x
  | isPrime x = [x]
  | otherwise = primeFactors' x 2
  where
    primeFactors' :: Int -> Int -> [ Int ]
    primeFactors' x' y
      | x' == 1         = []
      | x' `mod` y == 0 = y : primeFactors' (x' `div` y) y
      | otherwise       = primeFactors' x' nextTest
      where
        nextTest = y + 1 + y `mod` 2


splitDigits :: Int -> [Int]
-- Splits x from a (multi)digit number into a list of it's constituent digits
splitDigits x
  | x < 10  = [x]
  | otherwise = splitDigits (x `div` 10) ++ [x `mod` 10]


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
