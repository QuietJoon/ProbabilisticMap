{-# LANGUAGE CPP #-}

module ProbabilityDistribution.Data.Function where


import ProbabilityDistribution.Data

import qualified Data.IntMap as IM
import qualified Data.Map as M

#ifdef DEBUG
import Control.Exception
#endif

import Debug.Trace

-- Initialize functions

generateDPD :: [(Int,a)] -> Distribution a
generateDPD kvList = DPD bound (generateDPDSub 0 kvList IM.empty)
  where
    bound = sum $ map fst kvList
    generateDPDSub _ [] dist = dist
    generateDPDSub b ((k,v):rest) dist =
      generateDPDSub (b+k) rest (IM.insert b v dist)

generateCPD :: [(Double,a)] -> Distribution a
generateCPD kvList = CPD $ generateCPDSub 0 kvList M.empty
  where
    total = sum $ map fst kvList
    generateCPDSub _ [] dist = dist
    generateCPDSub c ((k,v):rest) dist =
      #ifdef DEBUG
      assert (c/total <= 1) $
      #endif
        generateCPDSub (c+k) rest (M.insert (c/total) v dist)

-- Merge functions

mergeDistribution :: Distribution a -> Distribution a -> Distribution a
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

mergeCPD (CPD dA) (CPD dB) rA rB = CPD newD
  where
    rA' = rA / (rA + rB)
    rB' = rB / (rA + rB)
    newDA = map (\(k,v) -> (k*rA',     v)) $ M.toList dA
    newDB = map (\(k,v) -> (k*rB'+rA', v)) $ M.toList dB
    newD = M.fromList $ newDA ++ newDB

mergeCPD _ _ _ _ = error "[ERROR] mergeCPD: Given wrong type of distribution"
