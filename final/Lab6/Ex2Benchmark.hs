import Criterion.Main
import Lab6
import Lecture6


--source: http://hackage.haskell.org/package/criterion-0.5.0.10/docs/Criterion-Main.html
main = defaultMain [
        bgroup "Our implementation" [ bench "mod 10" $ whnf (exM 10 20) 10, 
                                      bench "mod 10000" $ whnf (exM 10 10000) 10
                     ],

        bgroup "Haskell implementation" [ bench "mod 10" $ whnf (expM 10 20) 10,
                                          bench "mod 10000" $ whnf (expM 10 10000) 10
                     ]
                    ]


{-

Result interpretation: 
For large exponents modulo small base, the exM solution is at least ten times as fast, 
as the exponentiation is O(log n), instead of O(n) for regular solution.



Benchmark results: 

*Main> main
benchmarking Our implementation/mod 10
time                 6.244 μs   (6.207 μs .. 6.299 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 6.224 μs   (6.209 μs .. 6.250 μs)
std dev              66.46 ns   (43.02 ns .. 101.7 ns)

benchmarking Our implementation/mod 10000
time                 19.13 μs   (19.09 μs .. 19.16 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 19.17 μs   (19.12 μs .. 19.31 μs)
std dev              263.0 ns   (133.7 ns .. 494.5 ns)

benchmarking Haskell implementation/mod 10
time                 37.63 ns   (37.60 ns .. 37.69 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 37.65 ns   (37.62 ns .. 37.73 ns)
std dev              166.0 ps   (78.56 ps .. 305.0 ps)

benchmarking Haskell implementation/mod 10000
time                 448.7 ns   (446.9 ns .. 451.4 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 448.2 ns   (447.3 ns .. 449.7 ns)
std dev              3.908 ns   (2.572 ns .. 5.566 ns)


-}

