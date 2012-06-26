{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, FlexibleInstances, UndecidableInstances, Rank2Types, CPP #-}

#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE ConstraintKinds #-}
#else
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
#endif


{-# OPTIONS_HADDOCK hide #-}

module Data.PhaseChange.Internal where

import Control.Monad
import Control.Monad.ST        (ST, runST)
import Control.Monad.ST.Class
import GHC.Exts                (RealWorld)
--import Control.Newtype

class (Thawed imm ~ mut, Frozen mut ~ imm) => PhaseChange (imm :: *) (mut :: * -> *) where
    type Thawed imm  :: * -> *
    type Frozen mut  :: *
    unsafeThawImpl   :: imm   -> ST s (Thawed imm s)
    unsafeFreezeImpl :: mut s -> ST s (Frozen mut)
    copyImpl         :: mut s -> ST s (mut s)

-- the type synonyms look nicer in the haddocks, otherwise it doesn't matter which one we use
#if __GLASGOW_HASKELL__ >= 704
type Mutable   mut = PhaseChange (Frozen mut) mut
type Immutable imm = PhaseChange imm (Thawed imm)
#else
class    PhaseChange (Frozen mut) mut => Mutable mut
instance PhaseChange (Frozen mut) mut => Mutable mut

class    PhaseChange imm (Thawed imm) => Immutable imm
instance PhaseChange imm (Thawed imm) => Immutable imm
#endif

unsafeThaw :: (Immutable imm, MonadST mST, s ~ World mST) => imm -> mST (Thawed imm s)
unsafeThaw = liftST . unsafeThawImpl
{-# INLINABLE unsafeThaw #-}

unsafeFreeze :: (Mutable mut, MonadST mST, s ~ World mST) => mut s -> mST (Frozen mut)
unsafeFreeze = liftST . unsafeFreezeImpl
{-# INLINABLE unsafeFreeze #-}

copy :: (Mutable mut, MonadST mST, s ~ World mST) => mut s -> mST (mut s)
copy = liftST . copyImpl
{-# INLINABLE copy #-}
{-# SPECIALIZE copy :: (Mutable mut, s ~ World (ST s)) => mut s -> ST s (mut s) #-}
{-# SPECIALIZE copy :: (Mutable mut, s ~ RealWorld) => mut s -> IO (mut s) #-}
-- NOTE this is extremely delicate!
-- With IO it only works if I pre-expand the World type family, with ST it
-- only works if I don't. And if I use World directly in the original signature
-- instead of naming it 's', everything changes again.
-- GHC says things like: "RULE left-hand side too complicated to desugar",
-- or sometimes "match_co baling out"

thaw :: (Immutable imm, MonadST mST, s ~ World mST) => imm -> mST (Thawed imm s)
thaw = copy <=< unsafeThaw
{-# INLINABLE thaw #-}

freeze :: (Mutable mut, MonadST mST, s ~ World mST) => mut s -> mST (Frozen mut)
freeze = unsafeFreeze <=< copy
{-# INLINABLE freeze #-}
{-# SPECIALIZE freeze :: (Mutable mut, s ~ World (ST s)) => mut s -> ST s (Frozen mut) #-}
{-# SPECIALIZE freeze :: (Mutable mut, s ~ RealWorld) => mut s -> IO (Frozen mut) #-}

frozen :: Mutable mut => (forall s. ST s (mut s)) -> Frozen mut
frozen m = runST $ unsafeFreeze =<< m

updateWith :: Immutable imm => (forall s. Thawed imm s -> ST s ()) -> imm -> imm
updateWith f a = runST $ do { m <- thaw a; f m; unsafeFreeze m }
{-# INLINABLE updateWith #-}

updateWithResult :: Immutable imm => (forall s. Thawed imm s -> ST s a) -> imm -> (imm, a)
updateWithResult f a = runST $ do { m <- thaw a; r <- f m; i <- unsafeFreeze m; return (i, r) }
{-# INLINABLE updateWithResult #-}

--updateManyWith :: (Immutable imm, Functor f) => (forall s. f (Thawed imm s) -> ST s ()) -> f imm -> f imm
--updateManyWith f a = runST $ do { 

-- is this safe?
readWith :: Mutable mut => (Frozen mut -> a) -> mut s -> ST s a
readWith f a = do { i <- unsafeFreeze a; r <- return =<< (return $! (f $! i)); _ <- unsafeThaw i; return r }
{-# INLINABLE readWith #-}

--withPlus :: Immutable a => a -> (forall s. Thawed a s -> ST s (Thawed a s, b)) -> (a, b)



newtype M1 mut a s = M1 { unM1 :: mut s a }

--instance Newtype (M1 mut a s) (mut s a) where
--    pack   = M1
--    unpack = unM1

unsafeThaw1 :: (PhaseChange (imm a) (M1 mut a), MonadST mST, s ~ World mST) => imm a -> mST (mut s a)
unsafeThaw1 = liftM unM1 . unsafeThaw
{-# INLINE unsafeThaw1 #-}

unsafeFreeze1 :: (PhaseChange (imm a) (M1 mut a), MonadST mST, s ~ World mST) => mut s a -> mST (imm a)
unsafeFreeze1 = unsafeFreeze . M1
{-# INLINE unsafeFreeze1 #-}

copy1 :: (PhaseChange (imm a) (M1 mut a), MonadST mST, s ~ World mST) => mut s a -> mST (mut s a)
copy1 = liftM unM1 . copy . M1
{-# INLINE copy1 #-}

thaw1 :: (PhaseChange (imm a) (M1 mut a), MonadST mST, s ~ World mST) => imm a -> mST (mut s a)
thaw1 = liftM unM1 . thaw
{-# INLINE thaw1 #-}

freeze1 :: (PhaseChange (imm a) (M1 mut a), MonadST mST, s ~ World mST) => mut s a -> mST (imm a)
freeze1 = freeze . M1
{-# INLINE freeze1 #-}

updateWith1 :: PhaseChange (imm a) (M1 mut a) => (forall s. mut s a -> ST s ()) -> imm a -> imm a
updateWith1 f = updateWith (f . unM1)
{-# INLINE updateWith1 #-}


newtype M2 t a b s = M2 { unM2 :: t s a b }

--instance Newtype (M2 t a b s) (t s a b) where
--    pack   = M2
--    unpack = unM2

unsafeThaw2 :: (PhaseChange (imm a b) (M2 mut a b), MonadST mST, s ~ World mST) => imm a b -> mST (mut s a b)
unsafeThaw2 = liftM unM2 . unsafeThaw
{-# INLINE unsafeThaw2 #-}

unsafeFreeze2 :: (PhaseChange (imm a b) (M2 mut a b), MonadST mST, s ~ World mST) => mut s a b -> mST (imm a b)
unsafeFreeze2 = unsafeFreeze . M2
{-# INLINE unsafeFreeze2 #-}

copy2 :: (PhaseChange (imm a b) (M2 mut a b), MonadST mST, s ~ World mST) => mut s a b -> mST (mut s a b)
copy2 = liftM unM2 . copy . M2
{-# INLINE copy2 #-}

thaw2 :: (PhaseChange (imm a b) (M2 mut a b), MonadST mST, s ~ World mST) => imm a b -> mST (mut s a b)
thaw2 = liftM unM2 . thaw
{-# INLINE thaw2 #-}

freeze2 :: (PhaseChange (imm a b) (M2 mut a b), MonadST mST, s ~ World mST) => mut s a b -> mST (imm a b)
freeze2 = freeze . M2
{-# INLINE freeze2 #-}

updateWith2 :: PhaseChange (imm a b) (M2 mut a b) => (forall s. mut s a b -> ST s ()) -> imm a b -> imm a b
updateWith2 f = updateWith (f . unM2)
{-# INLINE updateWith2 #-}
