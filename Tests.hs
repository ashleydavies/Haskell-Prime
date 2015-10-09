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
    , TestCase "isCarmichael" isCarmichael
                              isCarmichaelTestCases
    , TestCase "nextSmithNumber" nextSmithNumber
                                 nextSmithNumberTestCases
    ]

runTests = mapM_ goTest allTestCases

main = runTests
