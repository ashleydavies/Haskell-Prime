module Recursion where

-- Precondition on all integers: they're all non-negative.

isPrime :: Int -> Bool
-- Determines the primality of a given number
isPrime x
    -- There are some special cases that are easier handled initially to simplify the core algorithm
    | x == 2          = True
    | x < 2 || even x = False
    | otherwise       = isPrime' 3
    where
        isPrime' :: Int -> Bool
        isPrime' y
            | y  > maxNum     = True
            --                Ensure not divisible by each y
            | x `mod` y == 0  = False
            | otherwise       = isPrime' (y + 2)
	        where
                -- Precalculate highest number to save time
		        maxNum = ceiling(sqrt(fromIntegral(x)))


nextPrime :: Int -> Int
-- Finds the next prime number after a given integer
nextPrime x
    -- Handle edge cases
    | x <= 1    = 2
    -- Add 2 if odd, 1 if even; allows to skip all even numbers as they are definitely not prime
    | otherwise = nextPrime' (x + 1 + x `mod` 2)
    where nextPrime' :: Int -> Int
          nextPrime' y
              | isPrime y = y
              | otherwise = nextPrime' (y + 2)


modPow :: Int -> Int -> Int -> Int
-- Pre: 1 <= m <= sqrt(maxint)
-- Calculates x^y mod n while minimising y to 1 for all exponential calculations so that
--  no integer overflow occurs for x <= maxint.

-- For even y, x^y mod n = (x^(y/2) mod n)^2 mod n
-- For odd  y, x^y mod n = (x mod n)(x^(y-1) mod n) mod n
modPow x y n
    | y <= 1    = x^y `mod` n
    | odd y     = ((x `mod` n) * (modPow x (y - 1) n)) `mod` n
    | even y    = (((modPow x (y `div` 2) n) `mod` n) ^ 2) `mod` n


isCarmichael :: Int -> Bool
-- Returns whether integer x is a carmichael number or not.
isCarmichael x
    | isPrime x = False
    | otherwise = isCarmichael' 2
    -- Check every number y between 2 and x - 1 to check if it passes the Fermat test for all y
    where isCarmichael' :: Int -> Bool
          isCarmichael' y 
            | y == x    = True
            | otherwise = (modPow y x x == y) && isCarmichael' (y + 1)
        


primeFactors :: Int -> [ Int ]
-- Pre: x >= 1
-- Returns a list of the prime factors for x
primeFactors x
    | isPrime x = [x]
    | otherwise = primeFactors' x 2 
    where primeFactors' :: Int -> Int -> [ Int ]
          primeFactors' x' y
            | x' == 1           = []
            | x' `mod` y == 0   = y : primeFactors' (x' `div` y) y
            | otherwise         = primeFactors' x' (y + 1 + y `mod` 2)


splitDigits :: Int -> [Int]
-- Splits x from a (multi)digit number into a list of it's component digits
splitDigits x
    | x < 10    = [x]
    | otherwise = splitDigits (x `div` 10) ++ [x `mod` 10]


sumDigits :: Int -> Int
-- Sums the digits of x using the splitDigits function
sumDigits x = sum $ splitDigits x


sumAllDigits :: [ Int ] -> Int
-- Applies sumDigits to a list
sumAllDigits x = sum $ map sumDigits x

isSmith :: Int -> Bool
-- Calculates and returns whether a given number x is a smith number or not.
isSmith x = sumDigits x == (sumAllDigits $ primeFactors x)

nextSmithNumber :: Int -> Int
-- Calculates the next smith number from x.
nextSmithNumber x
    | isSmith (x + 1) && (not $ isPrime (x + 1)) = x + 1
    | otherwise = nextSmithNumber (x + 1)
