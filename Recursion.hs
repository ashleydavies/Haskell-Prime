module Recursion where
import System.IO.Unsafe
import Debug.Trace
import Data.List
import Data.Ratio
import Data.Maybe
import System.Random

(//) :: (Integral a) => a -> a -> a
x // y = x `div` y
(%%) :: (Integral a) => a -> a -> a
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
-- Finds the next primeccept multiple types number after a given integer
nextPrime x
  | x <= 1  = 2   -- Allows the +2 optimisation on line +7 without any issue
  | odd x   = nextPrime' (x + 2)
  | even x  = nextPrime' (x + 1)
  where
    nextPrime' :: Int -> Int
    nextPrime' y
      | isPrime y = y
      | otherwise = nextPrime' (y + 2)


modPow :: (Integral a) => a -> a -> a -> a
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
-- It seems the euler totient takes far too long to calculate (and often
--   overflows) for certain numbers much larger, though many work acceptably.
-- Apologies for the few hacky functions; I didn't have enough time to
--   consider all of the mathematics behind them in time and had to rush
--   implement most of them before 12pm

greatestCD :: Int -> Int -> Int
-- Calculates the greatest common denominator of two integers
-- Pre: x, y > 0
greatestCD x y
  | x == y = x
  | x  > y = greatestCD (x - y) y
  | x  < y = greatestCD x (y - x)

modMultInverse :: Int -> Int -> Int
-- Returns the modular multiplicative index of a, b
-- Used in RSA for 'd', also ax = 1 mod m for a = eulTot n, m = e
modMultInverse a m
  | result < 0 = result + m
  | otherwise  = result
  where
    (result, _, _) = extendedGCDx a m

extendedGCDx :: Int -> Int -> (Int, Int, Int)
-- Algorithm followed closely from
--    http://rosettacode.org/wiki/Modular_inverse
-- Extended version of the greatest common denominator function
--   that returns a tuple of x, y, g to suit ax + by = g for g = gcd (a, b)
extendedGCDx m1 0 = (1, 0, m1)
extendedGCDx m1 m2
  = (t, s - q * t, g)
  where
    q         = m1 // m2
    r         = m1 %% m2
    (s, t, g) = extendedGCDx m2 r


uniqueIfy :: [Int] -> [Int]
-- Pre: SORTED in ascending integer order!
-- Takes a SORTED list of integers and "uniqueifies" it (removes duplicates)
uniqueIfy [] = []
uniqueIfy (x1:x2:xs)
  | x1 == x2  = uniqueIfy (x2:xs)
  | otherwise = x1 : (uniqueIfy (x2:xs))
uniqueIfy all@(x1:[]) = all


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


generatePrimePair :: (Int, Int)
-- Generate a random key pair. Used a hacky unsafe IO operation
--   because I couldn't figure out how to get it structured properly
--   to avoid doing so.
-- Returns the same numbers each session.
generatePrimePair = (prime1, prime2)
  where
    prime1 = nextPrime $ unsafePerformIO getRandomPrimeCandidate
    prime2 = nextPrime $ unsafePerformIO getRandomPrimeCandidate


getRandomPrimeCandidate :: IO Int
-- Super hacky way to get some random numbers that nextPrime will like =D
-- Bigger numbers works (Calculations begin to halt at ~1 million,
--   but take a very long time. This is just for examplar and testing use,
--   so a small number is fine.
getRandomPrimeCandidate = getStdRandom (randomR (500, 999))


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


rsaEncrypt :: Integer -> Int -> Int -> Integer
-- Given data, e and n, will encrypt data using RSA
rsaEncrypt dat e n
  = modPow dat (toInteger e) (toInteger n)


rsaDecrypt :: Integer -> Int -> Int -> Integer
-- Given data, d and n, will decrypt a number with RSA
rsaDecrypt dat d n
  = modPow dat (toInteger d) (toInteger n)


-- 2 is a safe number to start from for the public key exponent
basicEulN
  = eulTotient (getNpq generatePrimePair)
basicPublicKeyExp
  = getPublicKeyExponent 2 basicEulN
basicPrivateKeyExp
  = modMultInverse  basicPublicKeyExp basicEulN

rsaEncryptBasic :: Integer -> Integer
-- Encrypts data using the generated RSA key
rsaEncryptBasic dat
  = rsaEncrypt dat basicPublicKeyExp n
  where
    n = getNpq generatePrimePair
    

rsaDecryptBasic :: Integer -> Integer
-- Decrypts data using the generated RSA key
rsaDecryptBasic dat
  = rsaDecrypt dat basicPrivateKeyExp n
  where
    n = getNpq generatePrimePair
    d = modMultInverse en (getPublicKeyExponent 2 en)
    en = eulTotient n




cipherCharacters
  = (['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ " !\"£$%^&*()_+-=:")

basicCipherChar :: Char -> Int
-- Ciphers a character using the basic cipher above
-- Pre: c is contained in the cipher and isn't 
basicCipherChar c
  = (fromMaybe 0 (elemIndex c cipherCharacters)) + 10


basicDecipherChar :: Int -> Char
-- Undoes a cipher previously done
-- Pre: i is a ciphered character and hence is within the bounds of the list
basicDecipherChar i
  = cipherCharacters !! (i - 10)


basicCipherString :: String -> Integer
-- Ciphers an entire string of length n into an integer of 'length' 2n
-- Pre: Every character is a valid alphabetic character
basicCipherString str
  = basicCipherString' str 0
  where
    basicCipherString' :: String -> Integer -> Integer
    basicCipherString' [] ci      = ci
    basicCipherString' (x:xs) ci  = basicCipherString' xs (hund + ciphered)
      where
        ciphered = toInteger (basicCipherChar x)
        hund = ci * 100


basicDecipherString :: Integer -> String
-- Deciphers an entire integer of length 2n to a string of length n
-- Pre: Ciphered, so assumed constraints on digit values
basicDecipherString x
  = map basicDecipherChar pairs
  where
    pairs = splitPairs x
    splitPairs :: Integer -> [Int]
    splitPairs x
      | x < 100 = [fromIntegral(x)]
      | otherwise = splitPairs (x // 100) ++ [fromIntegral(x %% 100)]
