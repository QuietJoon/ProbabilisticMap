{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

module Data.ProbabilisticMap where


import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Map (Map)
import qualified Data.Map as M

import Text.Printf

#ifdef DEBUG
import Debug.Trace
import Control.Exception
#endif


data Distribution a
  = SD -- fixed, singleton
    { dSingleton :: a
    }
  | DPD -- Discrete probability Distribution
    { discBound :: {-# UNPACK #-} !Int
    , discDist :: IM.IntMap a
    }
  | CPD -- Continual probability Distribution
    { contDist :: M.Map Double a
    }


-- Show Instances

instance Show a => Show (Distribution a) where
  show (SD a)   = "SingD: " ++ show a
  show DPD {..} = "DiscPD(" ++ show discBound ++ "): " ++ showDisc discBound discDist
  show CPD {..} = "ContPD: " ++ showCont contDist

showDisc :: Show a => Int -> IntMap a -> String
showDisc discBound discDist = showDiscSub $ IM.toList discDist
  where
    showDiscSub [] = []
    showDiscSub (~(x,a):[]) = "[" ++ show (discBound - x) ++ "/" ++ show discBound ++ "]: " ++ show a
    showDiscSub ~(~(xk,a):y@(~(yk,_)):zs) = "[" ++ show (yk-xk) ++ "/" ++ show discBound ++ "]: " ++ show a ++ "\t" ++ showDiscSub (y:zs)

showCont :: Show a => Map Double a -> String
showCont discCont = showContSub $ M.toList discCont
  where
    showContSub [] = []
    showContSub (~(x,a):[]) = "[" ++ printf "%.4f" (1-x) ++ "]: " ++ show a
    showContSub ~(~(xk,a):y@(~(yk,_)):zs) = "[" ++ printf "%.4f" (yk-xk) ++ "]: " ++ show a ++ "\t" ++ showContSub (y:zs)


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
mergeDistribution (SD a) (SD b) =
#ifdef DEBUG
  trace "[Warn] mergeDistribution: Merge two singletones" $
#endif
    DPD 2 $ IM.fromList [(0,a),(1,b)]

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
