{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Pinned.DList where

import Foreign.Concurrent (newForeignPtr)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr, touchForeignPtr)
import Foreign.Marshal.Alloc (malloc, free)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable(..))
import GHC.Base (nullAddr#)
import GHC.Generics (Generic)
import GHC.Exts (IsList(..), realWorld#)
import GHC.ForeignPtr (ForeignPtr(..), newForeignPtr_)
import GHC.IO (IO(IO), unsafePerformIO)
import Control.Monad

unsafeInlineIO :: IO a -> a
unsafeInlineIO (IO m) = case m realWorld# of (# _, r #) -> r

nullForeignPtr :: ForeignPtr a
nullForeignPtr = ForeignPtr nullAddr# (error "nullForeignPtr")

data DList a = DList
  { listHead :: {-# UNPACK #-}!(ForeignPtr (Ptr (Node a))) -- First address
  , listLast :: {-# UNPACK #-}!(ForeignPtr (Ptr (Node a))) -- Last address
  }

instance (Storable a, Show a) => Show (DList a) where
  show = (:) '[' . unsafeInlineIO . foldrIO (\a b -> ',' : show a ++ b) "]"

data Node a = Node
  { nodeValue :: a
  , prevNode :: {-# UNPACK #-}!(Ptr (Node a))
  , nextNode :: {-# UNPACK #-}!(Ptr (Node a))
  }

instance Storable a => Storable (Node a) where
  sizeOf _ = sizeOf (undefined :: a) + 2 * sizeOf (undefined :: Ptr (Node a))
  alignment _ = alignment (undefined :: a)
  peek ptr = do
    nv <- peekByteOff ptr 0
    np <- peekByteOff ptr (sizeOf (undefined :: a))
    nn <-
      peekByteOff
        ptr
        (sizeOf (undefined :: a) + sizeOf (undefined :: Ptr (Node a)))
    return $ Node nv np nn
  poke ptr (Node nv np nn) = do
    pokeByteOff ptr 0 nv
    pokeByteOff ptr (sizeOf nv) np
    pokeByteOff ptr (sizeOf nv + sizeOf np) nn

empty :: DList a
empty = DList nullForeignPtr nullForeignPtr

singletonIO :: Storable a => a -> IO (DList a)
singletonIO a = do
  p <- malloc
  p `poke` Node a nullPtr nullPtr
  pph <- malloc
  ppt <- malloc
  pph `poke` p
  ppt `poke` p
  fph <- newForeignPtr pph (freeDLP pph)
  fpt <- newForeignPtr_ ppt
  return $ DList fph fpt

consIO :: Storable a => a -> DList a -> IO (DList a)
consIO a dl
  | listHead dl == nullForeignPtr = singletonIO a
  | otherwise = do
    let fph = listHead dl
    p <- malloc
    ph <- withForeignPtr fph peek
    p `poke` Node a nullPtr ph 
    pokeByteOff ph (sizeOf a) p
    withForeignPtr fph (`poke` p)
    return dl

freeDLP :: Storable a => Ptr (Ptr (Node a)) -> IO ()
freeDLP dlp
  | dlp == nullPtr = return ()
  | otherwise = go =<< peek dlp
    where
      go p
        | p == nullPtr = return ()
        | otherwise = do
            np <- nextNode <$> peek p
            free p
            go np

foldrIO :: Storable a => (a -> b -> b) -> b -> DList a -> IO b
foldrIO f z dl = do
  tp <- withForeignPtr (listLast dl) peek
  if tp == nullPtr
     then return z
     else do
       r <- go z =<< peek tp 
       touchForeignPtr (listHead dl)
       return r
  where
    go b (Node a px _)
      | px == nullPtr = return $ f a b 
      | otherwise = go (f a b) =<< peek px 







  {-
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Pinned.List where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Concurrent
import GHC.Exts
import Foreign.Storable
import GHC.ForeignPtr 
import GHC.IO

data Node a = Node
  { info :: a
  , prev :: {-# UNPACK #-}!(Ptr (Node a))
  , next :: {-# UNPACK #-}!(Ptr (Node a))
  }

instance Storable a => Storable (Node a) where
  sizeOf _ = sizeOf (undefined :: a) + 2 * sizeOf (undefined :: Ptr (Node a))
  alignment _ = alignment (undefined :: a)
  peek ptr = do
    nv <- peekByteOff ptr 0
    np <- peekByteOff ptr ( sizeOf (undefined :: a))
    nn <- peekByteOff ptr ( sizeOf (undefined :: a) 
                          + sizeOf (undefined :: Ptr (Node a)))
    return $ Node nv np nn
  poke ptr (Node nv np nn) = do
    pokeByteOff ptr 0 nv
    pokeByteOff ptr (sizeOf nv) np
    pokeByteOff ptr (sizeOf nv + sizeOf np) nn

data DList a = DList
  { hd :: ForeignPtr (Ptr (Node a))
  , tl :: ForeignPtr (Ptr (Node a))
  }

unsafeInlineIO :: IO a -> a
unsafeInlineIO (IO m) = case m realWorld# of (# _, r #) -> r

nullForeignPtr :: ForeignPtr a
nullForeignPtr = ForeignPtr nullAddr# (error "nullForeignPtr")

empty :: DList a
empty = DList nullForeignPtr nullForeignPtr 

cons :: Storable a => a -> DList a -> DList a
cons a dll = unsafeInlineIO $ do
  p <- malloc
  let fhp = hd dll
  hp <- withForeignPtr fhp peek
  h <- peek hp
  withForeignPtr fhp (`poke` p)
  p `poke` Node a nullPtr hp
  hp `poke` h { prev = p } 
  return dll






-}


