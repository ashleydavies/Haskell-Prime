module Tests where

import IC.TestSuite

import Recursion


isPrimeTestCases
  = [ 0 ==> False
    , 1 ==> False
    , 2 ==> True
    , 3 ==> True
    , 4 ==> False
    , 929 ==> True
    , 930 ==> False
    , (2^40 - 87) ==> True
    ]

nextPrimeTestCases
  = [ 0 ==> 2
    , 1 ==> 2
    , 2 ==> 3
    , 3 ==> 5
    , 920 ==> 929
    ]

modPowTestCases
  = [ (0, 0, 1) ==> 0
    , (1, 1, 1) ==> 0
    , (1, 1, 2) ==> 1
    , (13481, 11237, 6) ==> 5
    , (8, 0, 1) ==> 0
    , (8, 0, 5) ==> 1
    , (237, 1, 1000) ==> 237
    , (859237, 1, 1000) ==> 237
    , (33893, 2, 10000) ==> 5449
    , (7433893, 2, 10000) ==> 5449
    , (13481503, 11237126, 46340) ==> 6629
    ]

primeFactorsTestCases
  = [ 33554427 ==> [3, 641, 17449]
    , 268435451 ==> [71, 3780781]
    , 268370000 ==> [2, 2, 2, 2, 5, 5, 5, 5, 47, 571]
    ]

splitDigitsTestCases
  = [ 123456789 ==> [1, 2, 3, 4, 5, 6, 7, 8, 9]
    , 987654321 ==> [9, 8, 7, 6, 5, 4, 3, 2, 1]
    , 543216789 ==> [5, 4, 3, 2, 1, 6, 7, 8, 9]
    ]

sumDigitsTestCases
  = [ 123456789 ==> 45]

sumAllDigitsTestCases
  = [ [123, 456, 789] ==> 45
    , [100, 200, 300] ==> 6
    ]

isSmithTestCases
  = [ 4 ==> True
    , 4937775 ==> True
    , 22 ==> True
    , 666 ==> True -- spooky
    ]

isCarmichaelTestCases
  = [  3   ==> False
    ,  17  ==> False
    ,  341 ==> False
    ,  431 ==> False
    ,  561 ==> True
    ,  645 ==> False
    , 1105 ==> True
    , 1109 ==> False
    , 1387 ==> False
    , 1729 ==> True
    , 1905 ==> False
    , 2157 ==> False
    , 2465 ==> True
    ]

nextSmithNumberTestCases
  = [ 0       ==> 4
    , 4       ==> 22
    , 4937774 ==> 4937775
    , 4937750 ==> 4937775
    ]


allTestCases
  = [ TestCase "isPrime" isPrime
                         isPrimeTestCases
    , TestCase "nextPrime" nextPrime
                           nextPrimeTestCases
    , TestCase "modPow" (uncurry3 modPow)
                        modPowTestCases
    , TestCase "primeFactors" primeFactors
                              primeFactorsTestCases
    , TestCase "splitDigits" splitDigits
                             splitDigitsTestCases
    , TestCase "sumDigits" sumDigits
                           sumDigitsTestCases
    , TestCase "sumAllDigits" sumAllDigits
                              sumAllDigitsTestCases
    , TestCase "isSmith" isSmith
                         isSmithTestCases
    , TestCase "isCarmichael" isCarmichael
                              isCarmichaelTestCases
    , TestCase "nextSmithNumber" nextSmithNumber
                                 nextSmithNumberTestCases
    ]

runTests = mapM_ goTest allTestCases

main = runTests
