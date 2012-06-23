{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, FlexibleInstances, Rank2Types, MagicHash, UndecidableInstances, ScopedTypeVariables, GADTs, ConstraintKinds #-}

{-# OPTIONS_HADDOCK hide #-}

{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-name-shadowing #-}

module Data.PhaseChange.Instances () where

import Data.PhaseChange.Internal
import Control.Monad
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

-- | Data.Primitive.ByteArray
instance PhaseChange Prim.ByteArray Prim.MutableByteArray where
    type Thawed Prim.ByteArray        = Prim.MutableByteArray
    type Frozen Prim.MutableByteArray = Prim.ByteArray
    unsafeThawImpl   = unsafeThawByteArray
    unsafeFreezeImpl = unsafeFreezeByteArray
    copyImpl old = do
        let size = sizeofMutableByteArray old
        new <- newByteArray size
        copyMutableByteArray new 0 old 0 size
        return new

-- | Data.Primitive.Array
instance PhaseChange (Prim.Array a) (M1 Prim.MutableArray a) where
    type Thawed (Prim.Array a)           = M1 Prim.MutableArray a
    type Frozen (M1 Prim.MutableArray a) = Prim.Array a
    unsafeThawImpl   = liftM M1 . unsafeThawArray
    unsafeFreezeImpl = unsafeFreezeArray . unM1
    copyImpl mold = do
        let old  = unM1 mold
        let size = sizeofMutableArray old
        new <- Prim.newArray size (error "error")
        copyMutableArray new 0 old 0 size
        return (M1 new)

class    (Ix i, IArray iA a, MArray (stA s) a (ST s), iA i a ~ Frozen (M2 stA i a)) => ArrC iA stA s i a
instance (Ix i, IArray iA a, MArray (stA s) a (ST s), iA i a ~ Frozen (M2 stA i a)) => ArrC iA stA s i a

data C c where C :: c => C c

newtype S = S S -- do not export!!
anyS :: forall iA stA i a. ArrC iA stA S i a => (forall s. C (ArrC iA stA s i a))
anyS = unsafeCoerce (C :: C (ArrC iA stA S i a))

unsafeThawImpl_Arr :: forall iA stA s i a. ArrC iA stA S i a => iA i a -> ST s (stA s i a)
unsafeThawImpl_Arr a = case anyS of (C :: C (ArrC iA stA s i a)) -> (Arr.unsafeThaw a :: ST s (stA s i a))

unsafeFreezeImpl_Arr :: forall iA stA s i a. ArrC iA stA S i a => stA s i a -> ST s (iA i a)
unsafeFreezeImpl_Arr a = case anyS of (C :: C (ArrC iA stA s i a)) -> (Arr.unsafeFreeze a :: ST s (iA i a))

copyImpl_Arr :: forall iA stA s i a. ArrC iA stA S i a => stA s i a -> ST s (stA s i a)
copyImpl_Arr a = case anyS of (C :: C (ArrC iA stA s i a)) -> mapArray id a

-- fuck this is ugly

-- | Data.Array
instance (Ix i, IArray Arr.Array a, MArray (Arr.STArray S) a (ST S)) => PhaseChange (Arr.Array i a) (M2 Arr.STArray i a) where
    type Thawed (Arr.Array i a)      = M2 Arr.STArray i a
    type Frozen (M2 Arr.STArray i a) = Arr.Array i a
    unsafeThawImpl   = liftM M2 . unsafeThawImpl_Arr
    unsafeFreezeImpl = unsafeFreezeImpl_Arr . unM2
    copyImpl         = liftM M2 . copyImpl_Arr . unM2

-- | Data.Array.Unboxed
instance (Ix i, IArray Arr.UArray a, MArray (Arr.STUArray S) a (ST S)) => PhaseChange (Arr.UArray i a) (M2 Arr.STUArray i a) where
    type Thawed (Arr.UArray i a)      = M2 Arr.STUArray i a
    type Frozen (M2 Arr.STUArray i a) = Arr.UArray i a
    unsafeThawImpl   = liftM M2 . unsafeThawImpl_Arr
    unsafeFreezeImpl = unsafeFreezeImpl_Arr . unM2
    copyImpl         = liftM M2 . copyImpl_Arr . unM2

-- | Data.Vector
instance PhaseChange (Vec.Vector a) (M1 Vec.MVector a) where
    type Thawed (Vec.Vector a)     = M1 Vec.MVector a
    type Frozen (M1 Vec.MVector a) = Vec.Vector a
    unsafeThawImpl   = liftM M1 . Vec.unsafeThaw
    unsafeFreezeImpl = Vec.unsafeFreeze . unM1
    copyImpl         = liftM M1 . GVec.clone . unM1

-- | Data.Vector.Storable
instance Storable a => PhaseChange (SVec.Vector a) (M1 SVec.MVector a) where
    type Thawed (SVec.Vector a)     = M1 SVec.MVector a
    type Frozen (M1 SVec.MVector a) = SVec.Vector a
    unsafeThawImpl   = liftM M1 . SVec.unsafeThaw
    unsafeFreezeImpl = SVec.unsafeFreeze . unM1
    copyImpl         = liftM M1 . GVec.clone . unM1

-- | Data.Vector.Primitive
instance Prim a => PhaseChange (PVec.Vector a) (M1 PVec.MVector a) where
    type Thawed (PVec.Vector a)     = M1 PVec.MVector a
    type Frozen (M1 PVec.MVector a) = PVec.Vector a
    unsafeThawImpl   = liftM M1 . PVec.unsafeThaw
    unsafeFreezeImpl = PVec.unsafeFreeze . unM1
    copyImpl         = liftM M1 . GVec.clone . unM1

-- | Data.Vector.Unboxed
instance Unbox a => PhaseChange (UVec.Vector a) (M1 UVec.MVector a) where
    type Thawed (UVec.Vector a)     = M1 UVec.MVector a
    type Frozen (M1 UVec.MVector a) = UVec.Vector a
    unsafeThawImpl   = liftM M1 . UVec.unsafeThaw
    unsafeFreezeImpl = UVec.unsafeFreeze . unM1
    copyImpl         = liftM M1 . GVec.clone . unM1

{-
class Interfaceable a where
    iface :: IfaceRep a

icast :: Interfaceable a => a -> Maybe (Exists c)
-}

