{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, FlexibleInstances, UndecidableInstances, Rank2Types, ConstraintKinds #-}

{-# OPTIONS_HADDOCK hide #-}

module Data.PhaseChange.Internal where

import System.IO.Unsafe
import Control.Monad
import Control.Monad.ST        (ST, runST)
import Control.Monad.ST.Unsafe (unsafeSTToIO)
import Control.Monad.ST.Class
import Control.Newtype
import GHC.Exts                (Constraint)

class (Thawed i ~ m, Frozen m ~ i) => PhaseChange (i :: *) (m :: * -> *) where
    type Thawed i    :: * -> *
    type Frozen m    :: *
    unsafeThawImpl   :: i -> Thawed i s
    unsafeFreezeImpl :: m s -> Frozen m
    copyImpl         :: m s -> ST s (m s)

class    PhaseChange a (Thawed a) => Immutable a
instance PhaseChange a (Thawed a) => Immutable a

class    PhaseChange (Frozen a) a => Mutable a
instance PhaseChange (Frozen a) a => Mutable a

unsafePerformST :: ST s a -> a
unsafePerformST = unsafePerformIO . unsafeSTToIO

unsafeThaw :: Immutable a => a -> Thawed a s
unsafeThaw = unsafeThawImpl

unsafeFreeze :: Mutable a => a s -> Frozen a
unsafeFreeze = unsafeFreezeImpl

copy :: (MonadST m, Mutable a) => a (World m) -> m (a (World m))
copy = liftST . copyImpl
{-# INLINABLE copy #-}
{-# SPECIALIZE copy :: Mutable a => a (World (ST s)) -> ST s (a (World (ST s))) #-}
{-# SPECIALIZE copy :: Mutable a => a (World IO) -> IO (a (World IO)) #-}
-- need to keep the World in; otherwise, GHC says "RULE left-hand side too complicated to desugar"

thaw :: Immutable a => a -> Thawed a s
thaw = unsafePerformST . copyImpl . unsafeThaw
{-# INLINABLE thaw #-}

freeze :: (MonadST m, Mutable a) => a (World m) -> m (Frozen a)
freeze = liftM unsafeFreeze . copy
{-# INLINABLE freeze #-}
{-# SPECIALIZE freeze :: Mutable a => a (World (ST s)) -> ST s (Frozen a) #-}
{-# SPECIALIZE freeze :: Mutable a => a (World IO) -> IO (Frozen a) #-}

with :: Immutable a => a -> (forall s. Thawed a s -> ST s (Thawed a s)) -> a
with a f = runST $ liftM unsafeFreeze (f $ thaw a)
{-# INLINABLE with #-}

--withPlus :: Immutable a => a -> (forall s. Thawed a s -> ST s (Thawed a s, b)) -> (a, b)


class Empty a
instance Empty a

class (Thawed1 i ~ m, Frozen1 m ~ i) => PhaseChange1 (i :: * -> *) (m :: * -> * -> *) where
    type Thawed1 i    :: * -> * -> *
    type Frozen1 m    :: * -> *
    type C1 i :: * -> Constraint
    type C1 i = Empty
    unsafeThawImpl1   :: C1 i a => i a -> Thawed1 i s a
    unsafeFreezeImpl1 :: C1 i a => m s a -> Frozen1 m a
    copyImpl1         :: C1 i a => m s a -> ST s (m s a)

class    PhaseChange1 t (Thawed1 t) => Immutable1 t
instance PhaseChange1 t (Thawed1 t) => Immutable1 t

class    PhaseChange1 (Frozen1 t) t => Mutable1 t
instance PhaseChange1 (Frozen1 t) t => Mutable1 t

newtype I1 t a = I1 { unI1 :: t a }

newtype M1 t a s = M1 { unM1 :: t s a }

instance Newtype (I1 t a) (t a) where
    pack   = I1
    unpack = unI1

instance Newtype (M1 t a s) (t s a) where
    pack   = M1
    unpack = unM1

instance (PhaseChange1 i m, C1 i a) => PhaseChange (I1 i a) (M1 m a) where
    type Thawed (I1 i a) = M1 (Thawed1 i) a
    type Frozen (M1 m a) = I1 (Frozen1 m) a
    unsafeThawImpl       = M1      . unsafeThawImpl1   . unI1
    unsafeFreezeImpl     = I1      . unsafeFreezeImpl1 . unM1
    copyImpl             = fmap M1 . copyImpl1         . unM1
    {-# INLINE unsafeThawImpl   #-}
    {-# INLINE unsafeFreezeImpl #-}
    {-# INLINE copyImpl         #-}

type CM1 m = C1 (Frozen1 m)

unsafeThaw1 :: (Immutable1 t, C1 t a) => t a -> Thawed1 t s a
unsafeThaw1 = unM1 . unsafeThaw . I1
{-# INLINE unsafeThaw1 #-}

unsafeFreeze1 :: (Mutable1 t, CM1 t a) => t s a -> Frozen1 t a
unsafeFreeze1 = unI1 . unsafeFreeze . M1
{-# INLINE unsafeFreeze1 #-}

copy1 :: (MonadST m, Mutable1 t, CM1 t a) => t (World m) a -> m (t (World m) a)
copy1 = liftM unM1 . copy . M1
{-# INLINE copy1 #-}

thaw1 :: (Immutable1 t, C1 t a) => t a -> Thawed1 t s a
thaw1 = unM1 . thaw . I1
{-# INLINE thaw1 #-}

freeze1 :: (MonadST m, Mutable1 t, CM1 t a) => t (World m) a -> m (Frozen1 t a)
freeze1 = liftM unI1 . freeze . M1
{-# INLINE freeze1 #-}


class    Empty2 a b
instance Empty2 a b

class (Thawed2 i ~ m, Frozen2 m ~ i) => PhaseChange2 (i :: * -> * -> *) (m :: * -> * -> * -> *) where
    type Thawed2 i    :: * -> * -> * -> *
    type Frozen2 m    :: * -> * -> *
    type C2 i :: * -> * -> Constraint
    type C2 i = Empty2
    unsafeThawImpl2   :: C2 i a b => i a b -> Thawed2 i s a b
    unsafeFreezeImpl2 :: C2 i a b => m s a b -> Frozen2 m a b
    copyImpl2         :: C2 i a b => m s a b -> ST s (m s a b)

type CM2 m = C2 (Frozen2 m)

class    PhaseChange2 t (Thawed2 t) => Immutable2 t
instance PhaseChange2 t (Thawed2 t) => Immutable2 t

class    PhaseChange2 (Frozen2 t) t => Mutable2 t
instance PhaseChange2 (Frozen2 t) t => Mutable2 t

newtype I2 t a b = I2 { unI2 :: t a b }

newtype M2 t a b s = M2 { unM2 :: t s a b }

instance Newtype (I2 t a b) (t a b) where
    pack   = I2
    unpack = unI2

instance Newtype (M2 t a b s) (t s a b) where
    pack   = M2
    unpack = unM2

instance (PhaseChange2 i m, C2 i a b) => PhaseChange (I2 i a b) (M2 m a b) where
    type Thawed (I2 i a b) = M2 (Thawed2 i) a b
    type Frozen (M2 m a b) = I2 (Frozen2 m) a b
    unsafeThawImpl         = M2      . unsafeThawImpl2   . unI2
    unsafeFreezeImpl       = I2      . unsafeFreezeImpl2 . unM2
    copyImpl               = fmap M2 . copyImpl2         . unM2
    {-# INLINE unsafeThawImpl   #-}
    {-# INLINE unsafeFreezeImpl #-}
    {-# INLINE copyImpl         #-}

unsafeThaw2 :: (Immutable2 t, C2 t a b) => t a b -> Thawed2 t s a b
unsafeThaw2 = unM2 . unsafeThaw . I2
{-# INLINE unsafeThaw2 #-}

unsafeFreeze2 :: (Mutable2 t, CM2 t a b) => t s a b -> Frozen2 t a b
unsafeFreeze2 = unI2 . unsafeFreeze . M2
{-# INLINE unsafeFreeze2 #-}

copy2 :: (MonadST m, Mutable2 t, CM2 t a b) => t (World m) a b -> m (t (World m) a b)
copy2 = liftM unM2 . copy . M2
{-# INLINE copy2 #-}

thaw2 :: (Immutable2 t, C2 t a b) => t a b -> Thawed2 t s a b
thaw2 = unM2 . thaw . I2
{-# INLINE thaw2 #-}

freeze2 :: (MonadST m, Mutable2 t, CM2 t a b) => t (World m) a b -> m (Frozen2 t a b)
freeze2 = liftM unI2 . freeze . M2
{-# INLINE freeze2 #-}

