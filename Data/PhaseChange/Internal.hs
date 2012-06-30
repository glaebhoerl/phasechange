{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, Rank2Types, CPP #-}

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

-- | The @PhaseChange@ class ties together types which provide a mutable and an immutable view
--   on the same data. The mutable type must have a phantom type parameter representing the
--   state thread it is being used in. Many types have this type parameter in the wrong place
--   (not at the end): instances for them can be provided using the @'M1'@ and @'M2'@ newtypes.
class (Thawed imm ~ mut, Frozen mut ~ imm) => PhaseChange (imm :: *) (mut :: * -> *) where
    type Thawed imm  :: * -> *
    type Frozen mut  :: *
    -- | Should return the same data it got as input, viewed as a mutable type, making no
    --   changes.
    unsafeThawImpl   :: imm   -> ST s (Thawed imm s)
    -- | Should return the same data it got as input, viewed as an immutable type, making no
    --   changes.
    unsafeFreezeImpl :: mut s -> ST s (Frozen mut)
    -- | Should make a perfect copy of the input argument, leaving nothing shared between
    --   the original and the copy, and making no other changes.
    copyImpl         :: mut s -> ST s (mut s)

-- you might think that unsafeThaw and unsafeFreeze (and for that matter thaw) don't need to
-- be monadic, because after all they're just conceptually wrapping/unwrapping newtypes, and
-- the argument of thaw is immutable so evaluating lazily should be fine.
-- two things get in the way:
--  - GHC's unsafeFreeze/unsafeThaw aren't actually pure, they mutate some bits to let the
--    garbage collector know what's mutable and what's not; and
--  - while it wouldn't break referential transparency directly, it would if you were to use
--    the result of thaw/unsafeThaw in two different calls to runST. so we can't allow that.


#if __GLASGOW_HASKELL__ >= 704
type Mutable mut = PhaseChange (Frozen mut) mut
#else
class    PhaseChange (Frozen mut) mut => Mutable mut
instance PhaseChange (Frozen mut) mut => Mutable mut
#endif
-- the type synonyms look nicer in the haddocks, otherwise it doesn't matter which one we use

#if __GLASGOW_HASKELL__ >= 704
type Immutable imm = PhaseChange imm (Thawed imm)
#else
class    PhaseChange imm (Thawed imm) => Immutable imm
instance PhaseChange imm (Thawed imm) => Immutable imm
#endif

-- | Returns the input argument viewed as a mutable type. The input argument must not be used
--   afterwards.
unsafeThaw :: (Immutable imm, MonadST mST, s ~ World mST) => imm -> mST (Thawed imm s)
unsafeThaw = liftST . unsafeThawImpl
{-# INLINABLE unsafeThaw #-}
{-# SPECIALIZE unsafeThaw :: (Immutable imm, s ~ World (ST s)) => imm -> ST s (Thawed imm s) #-}
{-# SPECIALIZE unsafeThaw :: Immutable imm => imm -> IO (Thawed imm RealWorld) #-}
-- NOTE this is extremely delicate!
-- With IO it only works if I pre-expand the World type family, with ST it
-- only works if I don't. And if I use World directly in the original signature
-- instead of naming it 's', everything changes again.
-- In general the only way to figure it out seems to be trial and error.
-- Otherwise GHC says things like: "RULE left-hand side too complicated to desugar",
-- or sometimes "match_co baling out".

-- | Returns the input argument viewed as an immutable type. The input argument must not be used
--   afterwards.
unsafeFreeze :: (Mutable mut, MonadST mST, s ~ World mST) => mut s -> mST (Frozen mut)
unsafeFreeze = liftST . unsafeFreezeImpl
{-# INLINABLE unsafeFreeze #-}
{-# SPECIALIZE unsafeFreeze :: (Mutable mut, s ~ World (ST s)) => mut s -> ST s (Frozen mut) #-}
{-# SPECIALIZE unsafeFreeze :: (Mutable mut, s ~ RealWorld) => mut s -> IO (Frozen mut) #-}

-- | Make a copy of mutable data.
copy :: (Mutable mut, MonadST mST, s ~ World mST) => mut s -> mST (mut s)
copy = liftST . copyImpl
{-# INLINABLE copy #-}
{-# SPECIALIZE copy :: (Mutable mut, s ~ World (ST s)) => mut s -> ST s (mut s) #-}
{-# SPECIALIZE copy :: (Mutable mut, s ~ RealWorld) => mut s -> IO (mut s) #-}

-- | Get a copy of immutable data in mutable form.
thaw :: (Immutable imm, MonadST mST, s ~ World mST) => imm -> mST (Thawed imm s)
thaw = thawImpl
{-# INLINE thaw #-}

thawImpl :: (PhaseChange imm mut, MonadST mST) => imm -> mST (mut (World mST))
thawImpl = copy <=< unsafeThaw
{-# INLINABLE thawImpl #-}
{-# SPECIALIZE thawImpl :: PhaseChange imm mut => imm -> ST s (mut (World (ST s))) #-}
{-# SPECIALIZE thawImpl :: PhaseChange imm mut => imm -> IO (mut (World IO)) #-}
-- need to do this ugly thaw/thawImpl thing because I couldn't find any way at all to get
-- the SPECIALIZE to work otherwise
-- (interestingly unsafeThaw has the same exact type signature and didn't run have problems...)

-- | Get a copy of mutable data in immutable form.
freeze :: (Mutable mut, MonadST mST, s ~ World mST) => mut s -> mST (Frozen mut)
freeze = unsafeFreeze <=< copy
{-# INLINABLE freeze #-}
{-# SPECIALIZE freeze :: (Mutable mut, s ~ World (ST s)) => mut s -> ST s (Frozen mut) #-}
{-# SPECIALIZE freeze :: (Mutable mut, s ~ RealWorld) => mut s -> IO (Frozen mut) #-}

-- | Produce immutable data from a mutating computation. No copies are made.
frozen :: Mutable mut => (forall s. ST s (mut s)) -> Frozen mut
frozen m = runST $ unsafeFreeze =<< m
{-# NOINLINE [1] frozen #-}
-- {-# INLINABLE frozen #-}
-- I don't see why these should conflict, but GHC says they do

-- | Make an update of immutable data by applying a mutating action. This function allows for
--   copy elision.
-- 
--   Each chain of 'updateWith's makes only one copy. A chain of 'updateWith's on
--   top of a 'frozen' makes no copies.
updateWith :: Mutable mut => (forall s. mut s -> ST s ()) -> Frozen mut -> Frozen mut
updateWith f a = runST $ do { m <- thaw a; f m; unsafeFreeze m }
{-# NOINLINE [1] updateWith #-}
-- {-# INLINABLE updateWith #-}
-- really wanted to do this the other way around with Immutable and Thawed, but I found
-- absolutely no way to get the RULES to work that way, not even with the thaw/thawImpl trick

type Maker   mut = forall s. ST s (mut s)
type Updater mut = forall s. mut s -> ST s ()

{-# RULES
    "updateWith/frozen"
        forall (stm :: Maker mut) (f :: Updater mut).
           updateWith f (frozen stm) = frozen (stm >>= \m -> f m >> return m);

    "updateWith/updateWith"
         forall (f :: Updater mut) (g :: Updater mut) i.
              updateWith f (updateWith g i) = updateWith (\m -> f m >> g m) i
  #-}

updateWithResult :: Immutable imm => (forall s. Thawed imm s -> ST s a) -> imm -> (imm, a)
updateWithResult f a = runST $ do { m <- thaw a; r <- f m; i <- unsafeFreeze m; return (i, r) }
{-# INLINABLE updateWithResult #-}

{- RULES
    "updateWithResult/frozen"
        forall (m :: Mutable mut => (forall s. ST s (mut s))) (f :: Immutable imm => (forall s. Thawed imm s -> ST s a)).
           updateWithResult f (frozen m) = frozen (m >>= \m' -> f m' >> return m');
  -}

--updateManyWith :: (Immutable imm, Functor f) => (forall s. f (Thawed imm s) -> ST s ()) -> f imm -> f imm
--updateManyWith f a = runST $ do { 

--withPlus :: Immutable a => a -> (forall s. Thawed a s -> ST s (Thawed a s, b)) -> (a, b)

{-
let (foo, vec')   = updateWithResult asdf vec
    (bar, vec'')  = updateWithResult bsdf vec'
    (baz, vec''') = updateWithResult csdf vec''
-}

-- | Read a value from immutable data with a reading-computation on mutable data.
--   This function is referentially transparent as long as the computation does
--   not mutate its input argument, but there is no way to enforce this.
readWith :: Immutable imm => (forall s. Thawed imm s -> ST s a) -> imm -> a
readWith f i = runST $ do { m <- unsafeThaw i; r <- f m; _ <- unsafeFreeze m; return r }

-- | Newtype for mutable types whose state thread parameter is in the second-to-last position
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

frozen1 :: PhaseChange (imm a) (M1 mut a) => (forall s. ST s (mut s a)) -> imm a
frozen1 m = frozen (liftM M1 m)
{-# INLINE frozen1 #-}

updateWith1 :: PhaseChange (imm a) (M1 mut a) => (forall s. mut s a -> ST s ()) -> imm a -> imm a
updateWith1 f = updateWith (f . unM1)
{-# INLINE updateWith1 #-}

readWith1 :: PhaseChange (imm a) (M1 mut a) => (forall s. mut s a -> ST s b) -> imm a -> b
readWith1 f = readWith (f . unM1)
{-# INLINE readWith1 #-}

-- | Newtype for mutable types whose state thread parameter is in the third-to-last position
newtype M2 mut a b s = M2 { unM2 :: mut s a b }

--instance Newtype (M2 mut a b s) (mut s a b) where
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

frozen2 :: PhaseChange (imm a b) (M2 mut a b) => (forall s. ST s (mut s a b)) -> imm a b
frozen2 m = frozen (liftM M2 m)
{-# INLINE frozen2 #-}

updateWith2 :: PhaseChange (imm a b) (M2 mut a b) => (forall s. mut s a b -> ST s ()) -> imm a b -> imm a b
updateWith2 f = updateWith (f . unM2)
{-# INLINE updateWith2 #-}

readWith2 :: PhaseChange (imm a b) (M2 mut a b) => (forall s. mut s a b -> ST s c) -> imm a b -> c
readWith2 f = readWith (f . unM2)
{-# INLINE readWith2 #-}
