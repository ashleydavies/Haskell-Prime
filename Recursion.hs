module Recursion where

-- Precondition on all integers: they're all non-negative.

isPrime :: Int -> Bool
-- Determines the primality of a given number
isPrime x
    -- There are some special cases that are easier handled initially to simplify the core algorithm
    | x < 2          = False
    -- Only even prime
    | x == 2         = True
    -- Manually check all odd numbers up to the square root to conclusively determine primality
    | otherwise      = odd x && isPrime' 3
    where
        isPrime' :: Int -> Bool
        isPrime' y
            | y  > ceiling(sqrt(fromIntegral(x))) = True
            | x `mod` y == 0                      = False
            --                                    Ensure not divisble by each y
            | otherwise                           = isPrime' (y + 2)


nextPrime :: Int -> Int
-- Finds the next prime number after a given integer
nextPrime x
    -- Handle edge cases, and simplify core algorithm by ensuring only odd numbers get pushed in
    | x <= 1    = 2
    | even x    = nextPrime' (x + 1)
    | otherwise = nextPrime' (x + 2)
    -- Scans through the odd numbers looking for a prime number using the isPrime function
    -- Returns the first one it finds
    where nextPrime' :: Int -> Int
          nextPrime' y
              | isPrime y = y
              | otherwise = nextPrime' (y + 2)


modPow :: Int -> Int -> Int -> Int
-- Pre: 1 <= m <= sqrt(maxint)
-- Calculates x^y mod n while minimising y to 1 for all exponential calculations so that
--  no integer overflow occurs for x <= maxint.
modPow x y n
    -- For even y, x^y mod n = (x^(y/2) mod n)^2 mod n
    -- For odd  y, x^y mod n = (x mod n)(x^(y-1) mod n) mod n
    | y <= 1    = x^y `mod` n
    | odd y     = ((x `mod` n) * (modPow x (y - 1) n)) `mod` n
    | even y    = (((modPow x (y `div` 2) n) `mod` n) ^ 2) `mod` n


isCarmichael :: Int -> Bool
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
primeFactors x
    | isPrime x = [x]
    | otherwise = primeFactors' x 2 
    where primeFactors' :: Int -> Int -> [ Int ]
          primeFactors' x' y
            | x' == 1           = []
            | x' `mod` y == 0   = y : primeFactors' (x' `div` y) y
            | otherwise         = primeFactors' x' (nextPrime y)


splitDigits :: Int -> [Int]
splitDigits x
    | x < 10    = [x]
    | otherwise = splitDigits (x `div` 10) ++ [x `mod` 10]


sumDigits :: Int -> Int
sumDigits x = sum $ splitDigits x


sumAllDigits :: [ Int ] -> Int
sumAllDigits x = sum $ map sumDigits x


nextSmithNumber :: Int -> Int
nextSmithNumber = error "TODO: implement nextSmithNumber"
