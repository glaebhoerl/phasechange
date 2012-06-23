{-# LANGUAGE TypeFamilies, Trustworthy #-}

module Data.PhaseChange
    (
-- * Kind @*@
    PhaseChange  (type Thawed,  type Frozen),  Immutable,  Mutable,  copy,  thaw,  freeze, updateWith,
-- * Kind @* -> *@
    M1(..), copy1, thaw1, freeze1, updateWith1,
-- * Kind @* -> * -> *@
    M2(..), copy2, thaw2, freeze2, updateWith2
    )
    where

import Data.PhaseChange.Internal
import Data.PhaseChange.Instances ()
