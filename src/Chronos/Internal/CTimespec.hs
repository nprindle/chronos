{-# LANGUAGE CPP #-}
{-# LANGUAGE JavaScriptFFI #-}

{-# OPTIONS_HADDOCK hide #-} -- for doctests

module Chronos.Internal.CTimespec
  (
#ifdef mingw32_HOST_OS
    SystemTime(..)
  , getSystemTime
#else
    getPosixNanoseconds
  , CTimespec(..)
#endif
  ) where

import Foreign
import Foreign.C

#if defined(ghcjs_HOST_OS)

foreign import javascript unsafe "Date.now()" currentSeconds :: IO Double
getPosixNanoseconds :: IO Int64
getPosixNanoseconds = do
  x <- currentSeconds
  pure $ fromIntegral $ 1000000 * (round x)

#elif defined(mingw32_HOST_OS)

data SystemTime = SystemTime
  { wYear         :: {-# UNPACK #-} !Word16
  , wMonth        :: {-# UNPACK #-} !Word16
  , wDayOfWeek    :: {-# UNPACK #-} !Word16
  , wDay          :: {-# UNPACK #-} !Word16
  , wHour         :: {-# UNPACK #-} !Word16
  , wMinute       :: {-# UNPACK #-} !Word16
  , wSecond       :: {-# UNPACK #-} !Word16
  , wMilliseconds :: {-# UNPACK #-} !Word16
  }
  deriving (Eq, Show)

instance Storable SystemTime where
  sizeOf _ = 16
  alignment _ = alignment (undefined :: Word16)
  peek p = SystemTime
    <$> peekByteOff p 0
    <*> peekByteOff p 4
    <*> peekByteOff p 8
    <*> peekByteOff p 12
    <*> peekByteOff p 16
    <*> peekByteOff p 20
    <*> peekByteOff p 24
    <*> peekByteOff p 28
  poke p (SystemTime a b c d e f g h) = do
    pokeByteOff p 0  a
    pokeByteOff p 4  b
    pokeByteOff p 8  c
    pokeByteOff p 12 d
    pokeByteOff p 16 e
    pokeByteOff p 20 f
    pokeByteOff p 24 g
    pokeByteOff p 28 h

foreign import ccall unsafe "windows.h GetSystemTime"
  get_system_time :: Ptr SystemTime -> IO CInt

getSystemTime :: IO SystemTime
getSystemTime = alloca $ \p -> get_system_time p *> peek p

#else

data CTimespec = CTimespec
  { ctimespecSeconds :: {-# UNPACK #-} !CTime
  , ctimespecNanoseconds :: {-# UNPACK #-} !CLong
  }

instance Storable CTimespec where
    sizeOf _ = (16)
    alignment _ = alignment (undefined :: CLong)
    peek p = do
        s  <- (\hsc_ptr -> peekByteOff hsc_ptr 0) p
        ns <- (\hsc_ptr -> peekByteOff hsc_ptr 8) p
        return (CTimespec s ns)
    poke p (CTimespec s ns) = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) p s
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) p ns

#ifdef darwin_HOST_OS
foreign import ccall unsafe "cbits/hs-time.c clock_gettime"
    clock_gettime :: Int32 -> Ptr CTimespec -> IO CInt
#else
foreign import ccall unsafe "time.h clock_gettime"
    clock_gettime :: Int32 -> Ptr CTimespec -> IO CInt
#endif

-- | Get the current POSIX time from the system clock.
getPosixNanoseconds :: IO Int64
getPosixNanoseconds = do
  CTimespec (CTime s) (CLong ns) <- alloca $ \ptspec -> do
    throwErrnoIfMinus1_ "clock_gettime" $
      clock_gettime 0 ptspec
    peek ptspec
  return ((s * 1000000000) + ns)

#endif
