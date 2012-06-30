{-# LANGUAGE TypeFamilies, Trustworthy #-}

module Data.PhaseChange
    (
-- * @PhaseChange@ class
    PhaseChange (type Thawed, type Frozen),
-- * Assymetric constraint synonyms
    Mutable, Immutable,
-- * Functions
    thaw, freeze, copy, frozen, updateWith,
-- * Newtypes for shifting the \'s' type variable to the last position
    M1(..), M2(..),
-- * Convenience functions for working with @'M1'@
    thaw1, freeze1, copy1, frozen1, updateWith1,
-- * Convenience functions for working with @'M2'@
    thaw2, freeze2, copy2, frozen2, updateWith2
    )
    where

import Data.PhaseChange.Internal
import Data.PhaseChange.Instances ()
