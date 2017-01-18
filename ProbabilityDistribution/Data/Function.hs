-- Merge functions

module ProbabilityDistribution.Data.Function where


import ProbabilityDistribution.Data

import qualified Data.IntMap as IM
import qualified Data.Map as M

import Debug.Trace

mergeDistribution :: (Show a) => Distribution a -> Distribution a -> Distribution a
mergeDistribution (SD a) (SD b) = trace "[Warn] mergeDistribution: Merge two singletones"
                                  $ DPD 2 $ IM.fromList [(0,a),(1,b)]

mergeDistribution (DPD bA dA) (DPD bB dB)
  = DPD (bA + bB) newDist
  where
    newDListB = map (\(k,v) -> (k+bA, v)) $ IM.toList dB
    newDist = foldr (\(k,v) im -> IM.insert k v im) dA newDListB

mergeDistribution cpdA@(CPD _) cpdB@(CPD _)
  = mergeCPD cpdA cpdB 1 1

mergeDistribution _ _ = error "[ERROR] mergeDistribution: Each type of distribution are different"

mergeCPD (CPD dA) (CPD dB) rA rB = trace (show newDA ++ "\n" ++ show newDB) $ CPD newD
  where
    rA' = rA / (rA + rB)
    rB' = rB / (rA + rB)
    newDA = map (\(k,v) -> (k*rA',     v)) $ M.toList dA
    newDB = map (\(k,v) -> (k*rB'+rA', v)) $ M.toList dB
    newD = M.fromList $ newDA ++ newDB

mergeCPD _ _ _ _ = error "[ERROR] mergeCPD: Given wrong type of distribution"
