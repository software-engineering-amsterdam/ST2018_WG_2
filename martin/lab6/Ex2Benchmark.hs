import Criterion.Main
import Lab6
import Lecture6


--source: http://hackage.haskell.org/package/criterion-0.5.0.10/docs/Criterion-Main.html
main = defaultMain [
        bgroup "Our implementation" [ bench "10" $ whnf (exM 10 20) 10
                     ],

        bgroup "Haskell implementation" [ bench "10" $ whnf (expM 10 20) 10
                     ]
                    ]


