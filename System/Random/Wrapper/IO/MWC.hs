{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}

module System.Random.Wrapper.IO.MWC (
  module System.Random.Wrapper.IO.MWC,
  GenIO
  ) where


import System.Random.MWC

import Data.Time.Clock.POSIX
import Data.Vector (singleton)


-- Random generator nitializer

baseRG :: IO GenIO
baseRG = create

rgInitializer :: Int -> IO GenIO
rgInitializer = initialize . singleton . fromIntegral

rgInitializerSimple :: IO GenIO
rgInitializerSimple = do
  tSeed <- round <$> getPOSIXTime :: IO Int
  rgInitializer tSeed


-- randomizer

rBounded :: Variate a => (a, a) -> GenIO -> IO a
rBounded = uniformR
