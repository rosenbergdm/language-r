{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
-- Module      : fastsin_ffi
-- Copyright   : (c) 2010 David M. Rosenberg
-- License     : BSD3
-- 
-- Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
-- Stability   : experimental
-- Portability : portable
-- Created     : 06/12/10
-- 
-- Description :
--    DESCRIPTION HERE.
-----------------------------------------------------------------------------



import Foreign
import Foreign.C.Types

foreign import ccall unsafe "math.h sin" c_sin :: CDouble -> CDouble

sin :: Double -> Double
sin d = realToFrac (c_sin (realToFrac d))


