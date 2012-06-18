{-# LANGUAGE TypeFamilies, Trustworthy #-}

module Data.PhaseChange
    (
-- * Kind @*@
    PhaseChange  (type Thawed,  type Frozen),  Immutable,  Mutable,  copy,  thaw,  freeze,
-- * Kind @* -> *@
    PhaseChange1 (type Thawed1, type Frozen1, type C1), CM1, Immutable1, Mutable1, copy1, thaw1, freeze1, I1 (..), M1 (..),
-- * Kind @* -> * -> *@
    PhaseChange2 (type Thawed2, type Frozen2, type C2), CM2, Immutable2, Mutable2, copy2, thaw2, freeze2, I2 (..), M2 (..)
    )
    where

import Data.PhaseChange.Internal
import Data.PhaseChange.Instances ()
