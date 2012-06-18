{-# LANGUAGE Unsafe #-}

module Data.PhaseChange.Impl
    (
-- * Kind @*@
    PhaseChange(..),
-- * Kind @* -> *@
    PhaseChange1(..),
-- * Kind @* -> * -> *@
    PhaseChange2(..)
    )
    where

import Data.PhaseChange.Internal
import Data.PhaseChange.Instances ()
