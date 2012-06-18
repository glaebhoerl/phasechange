{-# LANGUAGE Unsafe #-}

module Data.PhaseChange.Unsafe
    (
-- * Kind @*@
    unsafeThaw,  unsafeFreeze,
-- * Kind @* -> *@
    unsafeThaw1, unsafeFreeze1,
-- * Kind @* -> * -> *@
    unsafeThaw2, unsafeFreeze2
    )
    where

import Data.PhaseChange.Internal
import Data.PhaseChange.Instances ()
