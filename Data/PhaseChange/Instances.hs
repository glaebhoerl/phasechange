{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, FlexibleInstances, Rank2Types, MagicHash, ConstraintKinds, EmptyDataDecls, UndecidableInstances, ScopedTypeVariables, GADTs #-}

{-# OPTIONS_HADDOCK hide #-}

{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-name-shadowing #-}

module Data.PhaseChange.Instances () where

import Data.PhaseChange.Internal
--import Control.Monad
import Control.Monad.ST
--import Control.Monad.Primitive
--import System.IO.Unsafe
import Unsafe.Coerce
import GHC.Exts

import Data.Primitive.Array        as Prim
import Data.Primitive.ByteArray    as Prim
import Data.Array                  as Arr
import Data.Array.ST               as Arr
import Data.Array.MArray           as Arr
import Data.Array.Unboxed          as Arr
import Data.Vector                 as Vec
import Data.Vector.Primitive       as PVec
import Data.Vector.Unboxed         as UVec
import Data.Vector.Storable        as SVec
import Data.Vector.Generic.Mutable as GVec

sizeofMutableArray :: Prim.MutableArray s a -> Int
sizeofMutableArray (MutableArray a) = I# (sizeofMutableArray# a)

-- | "Data.Primitive.ByteArray"
instance PhaseChange Prim.ByteArray Prim.MutableByteArray where
    type Thawed Prim.ByteArray        = Prim.MutableByteArray
    type Frozen Prim.MutableByteArray = Prim.ByteArray
    unsafeThawImpl   = unsafePerformST . unsafeThawByteArray
    unsafeFreezeImpl = unsafePerformST . unsafeFreezeByteArray
    copyImpl old = do
        let size = sizeofMutableByteArray old
        new <- newByteArray size
        copyMutableByteArray new 0 old 0 size
        return new

-- | "Data.Primitive.Array"
instance PhaseChange1 Prim.Array Prim.MutableArray where
    type Thawed1 Prim.Array        = Prim.MutableArray
    type Frozen1 Prim.MutableArray = Prim.Array
    unsafeThawImpl1   = unsafePerformST . unsafeThawArray
    unsafeFreezeImpl1 = unsafePerformST . unsafeFreezeArray
    copyImpl1 old = do
        let size = sizeofMutableArray old
        new <- Prim.newArray size (error "error")
        copyMutableArray new 0 old 0 size
        return new

class    (Ix i, IArray iA a, MArray (stA s) a (ST s)) => ArrC iA stA s i a
instance (Ix i, IArray iA a, MArray (stA s) a (ST s)) => ArrC iA stA s i a

data C c where C :: c => C c

data S
anyS :: forall iA stA i a. ArrC iA stA S i a => (forall s. C (ArrC iA stA s i a))
anyS = unsafeCoerce (C :: C (ArrC iA stA S i a))

unsafeThawImpl2_Arr :: forall iA stA s i a. ArrC iA stA S i a => iA i a -> stA s i a
unsafeThawImpl2_Arr a = case anyS of (C :: C (ArrC iA stA s i a)) -> unsafePerformST (Arr.unsafeThaw a :: ST s (stA s i a))

unsafeFreezeImpl2_Arr :: forall iA stA s i a. ArrC iA stA S i a => stA s i a -> iA i a
unsafeFreezeImpl2_Arr a = case anyS of (C :: C (ArrC iA stA s i a)) -> unsafePerformST (Arr.unsafeFreeze a :: ST s (iA i a))

copyImpl2_Arr :: forall stA s i a. ArrC (Frozen2 stA) stA S i a => stA s i a -> ST s (stA s i a)
copyImpl2_Arr a = case anyS of (C :: C (ArrC (Frozen2 stA) stA s i a)) -> mapArray id a

-- fuck this is ugly

-- | "Data.Array"
instance PhaseChange2 Arr.Array Arr.STArray where
    type Thawed2 Arr.Array   = Arr.STArray
    type Frozen2 Arr.STArray = Arr.Array
    type C2 Arr.Array = ArrC Arr.Array Arr.STArray S
    unsafeThawImpl2   = unsafeThawImpl2_Arr
    unsafeFreezeImpl2 = unsafeFreezeImpl2_Arr
    copyImpl2         = copyImpl2_Arr

-- | "Data.Array.Unboxed"
instance PhaseChange2 Arr.UArray Arr.STUArray where
    type Thawed2 Arr.UArray   = Arr.STUArray
    type Frozen2 Arr.STUArray = Arr.UArray
    type C2 Arr.UArray = ArrC Arr.UArray Arr.STUArray S
    unsafeThawImpl2    = unsafeThawImpl2_Arr
    unsafeFreezeImpl2  = unsafeFreezeImpl2_Arr
    copyImpl2          = copyImpl2_Arr

-- | "Data.Vector"
instance PhaseChange1 Vec.Vector Vec.MVector where
    type Thawed1 Vec.Vector  = Vec.MVector
    type Frozen1 Vec.MVector = Vec.Vector
    unsafeThawImpl1   = unsafePerformST . Vec.unsafeThaw
    unsafeFreezeImpl1 = unsafePerformST . Vec.unsafeFreeze
    copyImpl1         = GVec.clone

-- | "Data.Vector.Storable"
instance PhaseChange1 SVec.Vector SVec.MVector where
    type Thawed1 SVec.Vector  = SVec.MVector
    type Frozen1 SVec.MVector = SVec.Vector
    type C1 SVec.Vector = Storable
    unsafeThawImpl1   = unsafePerformST . SVec.unsafeThaw
    unsafeFreezeImpl1 = unsafePerformST . SVec.unsafeFreeze
    copyImpl1         = GVec.clone

-- | "Data.Vector.Primitive"
instance PhaseChange1 PVec.Vector PVec.MVector where
    type Thawed1 PVec.Vector  = PVec.MVector
    type Frozen1 PVec.MVector = PVec.Vector
    type C1 PVec.Vector = Prim
    unsafeThawImpl1   = unsafePerformST . PVec.unsafeThaw
    unsafeFreezeImpl1 = unsafePerformST . PVec.unsafeFreeze
    copyImpl1         = GVec.clone

-- | "Data.Vector.Unboxed"
instance PhaseChange1 UVec.Vector UVec.MVector where
    type Thawed1 UVec.Vector  = UVec.MVector
    type Frozen1 UVec.MVector = UVec.Vector
    type C1 UVec.Vector = Unbox
    unsafeThawImpl1   = unsafePerformST . UVec.unsafeThaw
    unsafeFreezeImpl1 = unsafePerformST . UVec.unsafeFreeze
    copyImpl1         = GVec.clone

{-
instance PhaseChange Array STArray
instance PhaseChange UArray STUArray
instance PhaseChange Vector MVector
instance Unbox  PhaseChange Unboxed

instances
STArray/Array, STUArray/UArray
Vector/MVector: boxed, Unboxed, Primitive, Storable


class Interfaceable a where
    iface :: IfaceRep a

icast :: Interfaceable a => a -> Maybe (Exists c)
-}

