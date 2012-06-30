{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Unsafe #-}
#endif

-- | This module provides functions on PhaseChangeable data which can break referential transparency if used incorrectly.
--   For safe functions, see "Data.PhaseChange". To write an instance, see "Data.PhaseChange.Impl".
module Data.PhaseChange.Unsafe
    (
-- * Unsafe functions
    unsafeThaw,  unsafeFreeze,  readWith,
-- * Convenience functions for working with @'M1'@
    unsafeThaw1, unsafeFreeze1, readWith1,
-- * Convenience functions for working with @'M2'@
    unsafeThaw2, unsafeFreeze2, readWith2
    )
    where

import Data.PhaseChange.Internal
import Data.PhaseChange.Instances ()
