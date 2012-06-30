{-# LANGUAGE TypeFamilies, Trustworthy #-}

-- | This module provides referentially transparent functions for working with PhaseChangeable data.
--   For functions that can break referential transparency, see "Data.PhaseChange.Unsafe".
--   If you want to write instances, see "Data.PhaseChange.Impl".
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

-- * A note on Safe Haskell
-- | Much like @Data.Typeable@, this module provides a class along with functions using it which are
--   safe as long as instances of the class play by the rules. This module is declared @Trustworthy@,
--   while "Data.PhaseChange.Impl" is @Unsafe@, so modules providing instances must necessarily also
--   be @Trustworthy@ (or @Unsafe@). It is up to the consumer to decide whether modules declaring
--   themselves @Trustworthy@ are actually to be trusted. The combination of any number of
--   @Trustworthy@ modules is safe only as long as all of them are.

-- * A note on GHC
-- | GHC doesn't handle the combination of @SPECIALIZE@ pragmas and type families very well. It appears
--   to be impossible to write them in a way that works with both GHC 7.2 and GHC 7.4. So here the ones
--   for freeze, thaw, etc. work with GHC 7.4 and spit a racket of warnings with 7.2. That's life.
    )
    where

import Data.PhaseChange.Internal
import Data.PhaseChange.Instances ()
