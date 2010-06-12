-----------------------------------------------------------------------------
-- Module      : cumsum_ffi
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

foreign import ccall unsafe "R_ext/Applic.h R_cumsum"
  c_R_cumsum :: 

