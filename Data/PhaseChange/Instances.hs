{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, FlexibleInstances, RankNTypes, MagicHash #-}

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

type WithMArray stArray s a = forall r. (MArray (stArray s) a (ST s) => r) -> r

mArray :: MArray (stArray s) a (ST s) => WithMArray stArray s a
mArray a = a

newtype S = S S --do not export!!

anyS :: WithMArray stArray S a -> WithMArray stArray s a
anyS = unsafeCoerce

hack :: MArray (stArray S) a (ST S) => ST s (M2 stArray i a s) -> WithMArray stArray s a
hack _ = anyS mArray

-- | Data.Array
instance (Ix i, IArray Arr.Array a, MArray (Arr.STArray S) a (ST S)) => PhaseChange (Arr.Array i a) (M2 Arr.STArray i a) where
    type Thawed (   Arr.Array   i a) = M2 Arr.STArray i a
    type Frozen (M2 Arr.STArray i a) =    Arr.Array   i a
    unsafeThawImpl   a = r where r = hack r (liftM M2 $ Arr.unsafeThaw a)
    unsafeFreezeImpl a = hack (return a) (Arr.unsafeFreeze $ unM2 a)
    copyImpl         a = hack (return a) (liftM M2 . mapArray id . unM2 $ a)

-- | Data.Array.Unboxed
instance (Ix i, IArray Arr.UArray a, MArray (Arr.STUArray S) a (ST S)) => PhaseChange (Arr.UArray i a) (M2 Arr.STUArray i a) where
    type Thawed (Arr.UArray i a)      = M2 Arr.STUArray i a
    type Frozen (M2 Arr.STUArray i a) = Arr.UArray i a
    unsafeThawImpl   a = r where r = hack r (liftM M2 $ Arr.unsafeThaw a)
    unsafeFreezeImpl a = hack (return a) (Arr.unsafeFreeze $ unM2 a)
    copyImpl         a = hack (return a) (liftM M2 . mapArray id . unM2 $ a)

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

