module Recursion where
import System.IO.Unsafe
import Debug.Trace
import Data.Ratio
import System.Random

(//) :: Int -> Int -> Int
x // y = x `div` y
(%%) :: Int -> Int -> Int
x %% y = x `mod` y

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
      | x %% y == 0  = False
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
  | y <= 1  = (x^y) %% n
  | odd y   = ((x %% n) * (modPow x (y - 1) n)) %% n
  | even y  = (((modPow x (y // 2) n) %% n) ^ 2) %% n


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
      | x' %% y == 0     = y : primeFactors' (x' // y) all
      | otherwise       = primeFactors' x' candidates


splitDigits :: Int -> [Int]
-- Splits x from a (multi)digit number into a list of it's constituent digits
splitDigits x
  | x < 10  = [x]
  | otherwise = splitDigits (x // 10) ++ [x %% 10]


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


---------------------
--  RSA Utilities  --
---------------------
-- I was in labs until 6.30pm trying to get this to work on huge primes
--   but gave up and had to settle for dealing with roughly ten thousand as
--   the max starting point for p and q.
-- It seems the euler totient takes far too long to calculate (and often
--   overflows) for certain numbers much larger, though many work acceptably.
-- Apologies for the few hacky functions; I didn't have enough time to
--   consider all of the mathematics behind them in time and had to rush
--   implement most of them.

greatestCD :: Int -> Int -> Int
-- Calculates the greatest common denominator of two integers
-- Pre: x, y > 0
greatestCD x y
  | x == y = x
  | x  > y = greatestCD (x - y) y
  | x  < y = greatestCD x (y - x)

getModMultInverse :: Int -> Int -> Int
-- Returns the modular multiplicative index of a, b
-- Used in RSA for 'd', also x in ax + by = gcd a, b for a = eulTot n, b = e
getModMultInverse a b = 2

uniqueIfy :: [Int] -> [Int]
-- Pre: SORTED in ascending integer order!
-- Takes a SORTED list of integers and "uniqueifies" it (removes duplicates)
uniqueIfy [] = []
uniqueIfy (x1:x2:xs)
  | x1 == x2  = uniqueIfy (x2:xs)
  | otherwise = x1 : (uniqueIfy (x2:xs))
uniqueIfy (x1:[]) = x1:[]

eulTotient :: Int -> Int
eulTotient 1 = 1
-- Pre: x > 0
-- Implementation of Euler's totient function (returns # coprimes to x (<x))

-- Strangely seems to break down on large (huge) numbers. Possibly due to Ratio
--   precision. Also tried float and double; same issues.
eulTotient x
  = numerator totient // denominator totient
  where
    totient = (x % 1) * eulTotient' ( uniqueIfy ( primeFactors x ))
    eulTotient' :: [Int] -> Ratio Int
    eulTotient' [] = 1 % 1
    eulTotient' (f:fs)
      = ( 1 % 1 - 1 % f) * eulTotient' fs


-- //// HACKY MAKESHIFT CODE 
generatePrimePair :: (Int, Int)
-- Generate a random key pair. Used a hacky unsafe IO operation
--   because I didn't have time to figure out how to get it structured properly
--   to avoid doing so.
-- Seems to return the same numbers every time until you recompile.
generatePrimePair = (prime1, prime2)
  where
    prime1 = nextPrime $ unsafePerformIO getRandomPrimeCandidate
    prime2 = nextPrime $ unsafePerformIO getRandomPrimeCandidate
-- HACKY MAKESHIFT CODE ////

getRandomPrimeCandidate :: IO Int
-- Super hacky way to get some random numbers that nextPrime will like =D
getRandomPrimeCandidate = getStdRandom (randomR (500, 9999))

getNpq :: (Int, Int) -> Int
-- Super simple function to get n from p and q. Purely for readability later
getNpq (p, q) = p * q

getPublicKeyExponent :: Int -> Int -> Int
-- Gets the public key exponent (e) between min and totient of n (see getNpq)
-- Must satisfy gcd(e, tot(n)) = 1 (i.e. they are coprime)
-- Low e are apparently somewhat less secure in some settings but should
--   be acceptable
getPublicKeyExponent min eulTotN
  | trace ("GPKE I " ++ show min) False   = 0
  | min == eulTotN   = error "Too big a minimum for public key exponent"
  | commonDenom == 1 = min
  | otherwise        = getPublicKeyExponent (min + 1) eulTotN
  where commonDenom  = greatestCD min eulTotN

rsaEncrypt :: Int -> Int -> Int -> Int
-- Given data, e and public key, will encrypt data using RSA
rsaEncrypt dat e pub
  = modPow dat e pub

rsaDecrypt :: Int -> Int -> Int -> Int
-- Given data, d and private key, will decrypt a number with RSA
rsaDecrypt dat d pri
  = modPow dat d pri
