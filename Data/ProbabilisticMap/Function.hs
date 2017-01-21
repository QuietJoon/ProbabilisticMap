{-# LANGUAGE CPP #-}

module Data.ProbabilisticMap.Function where


import Data.ProbabilisticMap

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Traversable as T

import System.Random.Wrapper.IO


materializeDistributionIO :: GenIO -> Distribution a -> IO a
materializeDistributionIO _ (SD s) = return s
materializeDistributionIO g (DPD b m) = do
  r <- rBounded (0,(b-1)) g :: IO Int
  return . snd . fromJust . IM.lookupLE r $ m
materializeDistributionIO g (CPD m) = do
  r <- rBounded (0,1) g :: IO Double
  return . snd . fromJust . M.lookupLE r $ m

materializeDistributionMapIO :: GenIO -> IntMap (Distribution a) -> IO (IntMap a)
materializeDistributionMapIO g =
  T.mapM (materializeDistributionIO g)

{-
materializeDistributionsIO :: GenIO -> Traversable (Distribution a) -> IO (Traversable a)
materializeDistributionsIO g =
  T.mapM (materializeDistributionIO g)
-}
