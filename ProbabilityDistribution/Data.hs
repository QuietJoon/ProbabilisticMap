{-# LANGUAGE RecordWildCards #-}

module ProbabilityDistribution.Data where


import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Map (Map)
import qualified Data.Map as M

import Text.Printf

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

instance Show a => Show (Distribution a) where
  show (SD a) = "SingD: " ++ show a
  show (DPD {..}) = "DiscPD(" ++ show discBound ++ "): " ++ showDisc discBound discDist
  show (CPD {..}) = "ContPD: " ++ showCont contDist

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
