module Test where


import Data.ProbabilisticMap
import Data.ProbabilisticMap.Function
import System.Random.Wrapper.IO


import Control.Monad

import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.Maybe
import qualified Data.Traversable as T

import Text.Printf

aDistributions =
  IM.fromList
    [ (1, SD "A")
    , (2, DPD 6 $ IM.fromList [(0,"B"),(1,"C")])
    , (3, CPD $ M.fromList [(0,"A"),(0.1,"B"),(0.5,"C")])
    ]

dpd1 = DPD 5 $ IM.fromList [(0,"A"),(2,"B")]
dpd2 = DPD 6 $ IM.fromList [(0,"C"),(3,"D")]
cpd1 = CPD $ M.fromList [(0,"A"),(0.1,"B"),(0.5,"C")]
cpd2 = CPD $ M.fromList [(0,"D"),(0.2,"E"),(0.7,"F")]

main = do
  let repeatNum = 100000
  gen <- create
  x <- replicateM repeatNum $ materializeDistributionMapIO gen aDistributions
  let y = map (fromJust . IM.lookup 2) x
      yb = length . filter (=="B") $ y
      yc = length . filter (=="C") $ y
      z = map (fromJust . IM.lookup 3) x
      za = length . filter (=="A") $ z
      zb = length . filter (=="B") $ z
      zc = length . filter (=="C") $ z
  print yb
  print yc
  printf "%.4f\n" $ ((fromIntegral yb) / (fromIntegral (yb+yc)) :: Double)
  printf "%.4f\n" $ ((1 / 6) :: Double)
  putStrLn "-----------"
  print za
  print zb
  print zc
  printf "%.4f\n" $ ((fromIntegral za) / (fromIntegral repeatNum) :: Double)
  printf "%.4f\n" $ ((fromIntegral zb) / (fromIntegral repeatNum) :: Double)
  printf "%.4f\n" $ ((fromIntegral zc) / (fromIntegral repeatNum) :: Double)