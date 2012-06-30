{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Unsafe #-}
#endif

-- | This module allows you to write instances for PhaseChangeable types.
--   To work with PhaseChangeable data, see "Data.PhaseChange". For unsafe functions, see "Data.PhaseChange.Unsafe".
module Data.PhaseChange.Impl
    (
    PhaseChange(..), M1(..), M2(..)
    )
    where

import Data.PhaseChange.Internal
import Data.PhaseChange.Instances ()
