module Main where

import Control.Monad ( liftM, liftM2 )
import Criterion.Main ( defaultMain, nfIO, bench, bgroup, nf, nfAppIO, env )
import Lib ( Point, multiply, multiplyNaive, multiplyVectorParallel, randomMatrix
           , perimeterNaive, perimeter, perimeterNaiveST, perimeterST
           , doubleAreaNaive, doubleArea, doubleAreaNaiveST, doubleAreaST, randomListOfPoints
           )

setupEnv :: IO ([[Int]], [[Int]], [[Int]], [[Int]], [[Int]], [[Int]], [Point])
setupEnv = do
  ms1 <- randomMatrix 300 300
  ms2 <- randomMatrix 300 300
  mm1 <- randomMatrix 1000 1000
  mm2 <- randomMatrix 1000 1000
  ml1 <- randomMatrix 100 1000
  ml2 <- randomMatrix 1000 3000
  poly <- randomListOfPoints 10000000
  return (ms1, ms2, mm1, mm2, ml1, ml2, poly)

main :: IO ()
main = defaultMain 
  [ env setupEnv $ \ ~(ms1, ms2, mm1, mm2, ml1, ml2, poly) -> bgroup "tests"
    [ bgroup "Matrix multiplication" 
      [ bgroup "parallel" 
        [ bench "300 x 300" $ nf (multiply ms1) ms2
        , bench "1000 x 1000" $ nf (multiply mm1) mm2
        , bench "100 x 1000 * 1000 x 3000" $ nf (multiply ml1) ml2
        ]
      , bgroup "sequential" 
        [ bench "300 x 300" $ nf (multiplyNaive ms1) ms2
        , bench "1000 x 1000" $ nf (multiplyNaive mm1) mm2
        , bench "100 x 1000 * 1000 x 3000" $ nf (multiplyNaive ml1) ml2
        ]
      , bgroup "parallel vector" 
        [ bench "300 x 300" $ nf (multiplyVectorParallel ms1) ms2
        , bench "1000 x 1000" $ nf (multiplyVectorParallel mm1) mm2
        , bench "100 x 1000 * 1000 x 3000" $ nf (multiplyVectorParallel ml1) ml2
        ]
      ]
    , bgroup "Polygon perimeter" 
      [ bgroup "naive" 
        [ bench "10000000" $ nf perimeterNaive poly
        ]
      , bgroup "iterative" 
        [ bench "10000000" $ nf perimeter poly
        ]
      , bgroup "naive with ST" 
        [ bench "10000000" $ nf perimeterNaiveST poly
        ]
      , bgroup "iterative with ST" 
        [ bench "10000000" $ nf perimeterST poly
        ]
      ]  
    , bgroup "Polygon double area" 
      [ bgroup "naive" 
        [ bench "10000000" $ nf doubleAreaNaive poly
        ]
      , bgroup "iterative" 
        [ bench "10000000" $ nf doubleArea poly
        ]
      , bgroup "naive with ST" 
        [ bench "10000000" $ nf doubleAreaNaiveST poly
        ]
      , bgroup "iterative with ST" 
        [ bench "10000000" $ nf doubleAreaST poly
        ]
      ]  
    ]
  ]