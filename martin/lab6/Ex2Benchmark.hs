import Criterion.Main
import Lab6
import Lecture6


--source: http://hackage.haskell.org/package/criterion-0.5.0.10/docs/Criterion-Main.html
main = defaultMain [
        bgroup "Our implementation" [ bench "10" $ whnf (exM 10 20) 10, 
        							  bench "100" $ whnf (exM 10 100) 10
                     ],

        bgroup "Haskell implementation" [ bench "10" $ whnf (expM 10 20) 10,
        								  bench "100" $ whnf (expM 10 100) 10
                     ]
                    ]


{-
*Main> main
benchmarking Our implementation/10
time                 6.240 μs   (6.228 μs .. 6.252 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 6.228 μs   (6.213 μs .. 6.258 μs)
std dev              67.11 ns   (37.69 ns .. 122.5 ns)

benchmarking Haskell implementation/10
time                 37.78 ns   (37.70 ns .. 37.87 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 37.93 ns   (37.77 ns .. 38.37 ns)
std dev              761.1 ps   (387.1 ps .. 1.406 ns)
variance introduced by outliers: 29% (moderately inflated)

-}

