{-# LANGUAGE CPP #-}

module System.Random.Wrapper.IO (
  module WrapperIO
  ) where

#ifdef SFMT
import System.Random.Wrapper.IO.SFMT as WrapperIO
#else
import System.Random.Wrapper.IO.MWC  as WrapperIO
#endif
