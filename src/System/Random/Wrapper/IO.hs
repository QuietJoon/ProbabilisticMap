{-# LANGUAGE CPP #-}

module System.Random.Wrapper.IO (
  module WrapperIO
  ) where

#ifdef SFMT
import System.Random.SFMT as WrapperIO
#else
import System.Random.MWC  as WrapperIO
#endif
