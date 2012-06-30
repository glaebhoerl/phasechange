{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, FlexibleInstances, RankNTypes, MagicHash, UnboxedTuples #-}

{-# OPTIONS_HADDOCK hide #-}

{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-name-shadowing -fno-warn-unused-binds #-}

module Data.PhaseChange.Instances () where

import Data.PhaseChange.Internal
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Unsafe.Coerce
import GHC.Exts

-- they sure made a big mess out of the Array modules...
import Data.Primitive.Array        as Prim
import Data.Primitive.ByteArray    as Prim
import Data.Array                  as Arr  (Array)
import Data.Array.ST               as Arr  (STArray, STUArray)
import Data.Array.Unboxed          as Arr  (UArray)
import Data.Array.IArray           as Arr  (IArray, Ix)
import Data.Array.MArray           as Arr  (MArray, mapArray)
import Data.Array.Unsafe           as Arr  (unsafeThaw, unsafeFreeze)
import Data.Vector                 as Vec
import Data.Vector.Primitive       as PVec
import Data.Vector.Unboxed         as UVec
import Data.Vector.Storable        as SVec
import Data.Vector.Generic.Mutable as GVec

cloneMutableArray :: (PrimMonad m, s ~ PrimState m) => MutableArray s a -> Int -> Int -> m (MutableArray s a)
cloneMutableArray (MutableArray a#) (I# begin#) (I# size#) =
    primitive $ \s# -> case cloneMutableArray# a# begin# size# s#
                       of (# s'#, a'# #) -> (# s'#, MutableArray a'# #)

sizeofMutableArray :: MutableArray s a -> Int
sizeofMutableArray (MutableArray a#) = I# (sizeofMutableArray# a#)

-- * primitive

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
    copyImpl (M1 a)  = liftM M1 $ cloneMutableArray a 0 (sizeofMutableArray a)


-- * array

-- NOTE
-- for the Array types, we have to use a hack: we want to write "forall s. MArray (STArray s) a (ST s)"
-- in the instance declaration, but we can't do that. our hack is that we have an unexported type, S,
-- and we write "MArray (STArray S) a (ST S)" instead. because S is not exported, the only way the
-- constraint can be satisfied is if it is true forall s. and then we use unsafeCoerce.
-- (this trick is borrowed from Edward Kmett's constraints library)

-- capture and store the evidence for an MArray constraint in CPS form
type WithMArray stArray s a = forall r. (MArray (stArray s) a (ST s) => r) -> r

-- capture locally available evidence and store it
mArray :: MArray (stArray s) a (ST s) => WithMArray stArray s a
mArray a = a

-- see NOTE above. do not export!
newtype S = S S

-- if we know MArray for S, it must be true forall s. make it so.
anyS :: WithMArray stArray S a -> WithMArray stArray s a
anyS = unsafeCoerce

-- from locally available evidence of MArray for S, produce evidence we can use
-- for any s. the first argument is just a dummy to bring type variables into scope,
-- chosen to be convenient for the particular use sites that we have.
hack :: MArray (stArray S) a (ST S) => ST s (M2 stArray i a s) -> WithMArray stArray s a
hack _ = anyS mArray

-- | Data.Array
instance (Ix i, IArray Arr.Array a, MArray (Arr.STArray S) a (ST S)) => PhaseChange (Arr.Array i a) (M2 Arr.STArray i a) where
    type Thawed (Arr.Array i a)      = M2 Arr.STArray i a
    type Frozen (M2 Arr.STArray i a) = Arr.Array i a
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


-- * vector

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

