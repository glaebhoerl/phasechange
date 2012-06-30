{-# LANGUAGE Unsafe #-}

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
