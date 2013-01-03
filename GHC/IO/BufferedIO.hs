{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IO.BufferedIO
-- Copyright   :  (c) The University of Glasgow 2008
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Class of buffered IO devices
--
-----------------------------------------------------------------------------

module GHC.IO.BufferedIO (
        BufferedIO(..),
        defaultReadBuffered,
        defaultReadBuffered0,
        defaultWriteBuffered,
        defaultWriteBuffered0,

        -- * Implementing BufferedIO using RawIO
        -- $rawio
        defaultBufferSize,
        readBuf,
        readBufNonBlocking,
        rawIOReadBuffered,
        rawIOReadBuffered0,
        writeBuf,
        writeBufNonBlocking,
        rawIOWriteBuffered,
        rawIOWriteBuffered0,
    ) where

import GHC.Base
import GHC.Ptr
import GHC.Real
import Data.Word
import GHC.Num
import Data.Maybe
import GHC.IO.Device as IODevice
import GHC.IO.Device as RawIO
import GHC.IO.Buffer

import Foreign.C.Types

-- | The purpose of 'BufferedIO' is to provide a common interface for I/O
-- devices that can read and write data through a buffer.  Devices that
-- implement 'BufferedIO' include ordinary files, memory-mapped files,
-- and bytestrings.  The underlying device implementing a 'Handle' must
-- provide 'BufferedIO'.
--
-- If your device supports 'RawIO', you can easily implement 'BufferedIO'
-- using functions provided by this module; see below.
class BufferedIO dev where
  -- | allocate a new buffer.  The size of the buffer is at the
  -- discretion of the device; e.g. for a memory-mapped file the
  -- buffer will probably cover the entire file.
  newBuffer         :: dev -> BufferState -> IO (Buffer Word8)

  -- | reads bytes into the buffer, blocking if there are no bytes
  -- available.  Returns the number of bytes read (zero indicates
  -- end-of-file), and the new buffer.
  fillReadBuffer    :: dev -> Buffer Word8 -> IO (Int, Buffer Word8)

  -- | reads bytes into the buffer without blocking.  Returns the
  -- number of bytes read (Nothing indicates end-of-file), and the new
  -- buffer.
  fillReadBuffer0   :: dev -> Buffer Word8 -> IO (Maybe Int, Buffer Word8)

  -- | Read some bytes, copying them to the given address.  Block only if no
  -- data is available yet.  Return 0 to signal EOF.
  --
  -- 'readBuffered' may return a short count even if more data is immediately available.
  -- However, it may only block if the buffer is empty and the underlying
  -- device is not ready to read.
  --
  -- The default implementation uses 'fillReadBuffer'.  If the device supports
  -- 'RawIO', use @readBuffered = 'rawIOReadBuffered'@ to avoid unnecessary
  -- copying for large reads.
  --
  -- 'readBuffered' is never called with count <= 0.
  readBuffered :: dev -> Buffer Word8 -> Ptr Word8 -> Int -> IO (Int, Buffer Word8)
  readBuffered = defaultReadBuffered

  -- | Non-blocking version of 'readBuffered'.  Return 'Nothing' to signal EOF.
  -- Return @'Just' 0@ if and only if a call to 'readBuffered' with the same
  -- parameters would block.
  --
  -- 'readBuffered0' is never called with count <= 0.
  readBuffered0 :: dev -> Buffer Word8 -> Ptr Word8 -> Int -> IO (Maybe Int, Buffer Word8)
  readBuffered0 = defaultReadBuffered0

  -- | Prepares an empty write buffer.  This lets the device decide
  -- how to set up a write buffer: the buffer may need to point to a
  -- specific location in memory, for example.  This is typically used
  -- by the client when switching from reading to writing on a
  -- buffered read/write device.
  --
  -- There is no corresponding operation for read buffers, because before
  -- reading the client will always call 'fillReadBuffer'.
  emptyWriteBuffer  :: dev -> Buffer Word8 -> IO (Buffer Word8)
  emptyWriteBuffer _dev buf 
    = return buf{ bufL=0, bufR=0, bufState = WriteBuffer }

  -- | Flush all the data from the supplied write buffer out to the device.
  -- The returned buffer should be empty, and ready for writing.
  flushWriteBuffer  :: dev -> Buffer Word8 -> IO (Buffer Word8)

  -- | Flush data from the supplied write buffer out to the device
  -- without blocking.  Returns the number of bytes written and the
  -- remaining buffer.
  flushWriteBuffer0 :: dev -> Buffer Word8 -> IO (Int, Buffer Word8)

  -- | Write all of the given bytes.
  --
  -- If @flushAfter@ is 'True', flush after writing, returning an
  -- empty 'Buffer'.
  --
  -- The default implementation uses 'flushWriteBuffer'.  If the device
  -- supports 'RawIO', use @writeBuffered = 'rawIOWriteBuffered'@ to avoid
  -- unnecessary copying for large writes.
  --
  -- 'writeBuffered' is never called with count <= 0.
  writeBuffered :: dev
                -> Bool             -- ^ @flushAfter@
                -> Buffer Word8
                -> Ptr Word8
                -> Int
                -> IO (Buffer Word8)
  writeBuffered = defaultWriteBuffered

  -- | Non-blocking version of 'writeBuffered'.  Returns the number of
  -- bytes written.
  --
  -- If @flushAfter@ is 'True', any bytes copied to the buffer by this method
  -- must be flushed out.
  --
  -- If the underlying device is ready to be written to, 'writeBuffered0' must
  -- make progress, either by copying bytes to the buffer, writing bytes to
  -- the device, or both.
  --
  -- Warning: the default implementation uses 'flushWriteBuffer'
  -- (blocking flush) when @flushAfter@ is 'True', so that bytes written to the
  -- buffer are actually sent.  If your 'flushWriteBuffer' method could
  -- potentially block, consider using 'rawIOWriteBuffered0' instead, which
  -- does not have this problem.
  --
  -- 'writeBuffered0' is never called with count <= 0.
  writeBuffered0 :: dev
                 -> Bool -> Buffer Word8 -> Ptr Word8 -> Int
                 -> IO (Int, Buffer Word8)
  writeBuffered0 = defaultWriteBuffered0

-- for an I/O device, these operations will perform reading/writing
-- to/from the device.

-- for a memory-mapped file, the buffer will be the whole file in
-- memory.  fillReadBuffer sets the pointers to encompass the whole
-- file, and flushWriteBuffer needs to do no I/O.  A memory-mapped
-- file has to maintain its own file pointer.

-- for a bytestring, again the buffer should match the bytestring in
-- memory.

-- ---------------------------------------------------------------------------
-- Low-level read/write to/from buffers

{- $rawio

These definitions make it easy to implement an instance of 'BufferedIO'
for an object that supports 'RawIO'.  For example, here is how the
'BufferedIO' instance for 'FD' is defined:

@
instance 'BufferedIO' 'FD' where
  'newBuffer' _dev state = 'newByteBuffer' 'defaultBufferSize' state
  'fillReadBuffer'    = 'readBuf'
  'fillReadBuffer0'   = 'readBufNonBlocking'
  'readBuffered'      = 'rawIOReadBuffered'
  'readBuffered0'     = 'rawIOReadBuffered0'
  'flushWriteBuffer'  = 'writeBuf'
  'flushWriteBuffer0' = 'writeBufNonBlocking'
  'writeBuffered'     = 'rawIOWriteBuffered'
  'writeBuffered0'    = 'rawIOWriteBuffered0'
@
-}

-- We used to use System.Posix.Internals.dEFAULT_BUFFER_SIZE, which is
-- taken from the value of BUFSIZ on the current platform.  This value
-- varies too much though: it is 512 on Windows, 1024 on OS X and 8192
-- on Linux.  So let's just use a decent size on every platform:
defaultBufferSize :: Int
defaultBufferSize = 8192

readBuf :: RawIO dev => dev -> Buffer Word8 -> IO (Int, Buffer Word8)
readBuf dev bbuf = do
  let bytes = bufferAvailable bbuf
  res <- withBuffer bbuf $ \ptr ->
             RawIO.read dev (ptr `plusPtr` bufR bbuf) bytes
  return (res, bbuf{ bufR = bufR bbuf + res })
         -- zero indicates end of file

readBufNonBlocking :: RawIO dev => dev -> Buffer Word8
                     -> IO (Maybe Int,   -- Nothing ==> end of file
                                         -- Just n  ==> n bytes were read (n>=0)
                            Buffer Word8)
readBufNonBlocking dev bbuf = do
  let bytes = bufferAvailable bbuf
  res <- withBuffer bbuf $ \ptr ->
           IODevice.readNonBlocking dev (ptr `plusPtr` bufR bbuf) bytes
  case res of
     Nothing -> return (Nothing, bbuf)
     Just n  -> return (Just n, bbuf{ bufR = bufR bbuf + n })

writeBuf :: RawIO dev => dev -> Buffer Word8 -> IO (Buffer Word8)
writeBuf dev bbuf = do
  let bytes = bufferElems bbuf
  withBuffer bbuf $ \ptr ->
      IODevice.write dev (ptr `plusPtr` bufL bbuf) bytes
  return bbuf{ bufL=0, bufR=0 }

writeBufNonBlocking :: RawIO dev => dev -> Buffer Word8 -> IO (Int, Buffer Word8)
writeBufNonBlocking dev bbuf = do
  let bytes = bufferElems bbuf
  res <- withBuffer bbuf $ \ptr ->
            IODevice.writeNonBlocking dev (ptr `plusPtr` bufL bbuf) bytes
  return (res, bufferAdjustL (bufL bbuf + res) bbuf)

-- ---------------------------------------------------------------------------
-- readBuffered

-- | Default implementation of 'readBuffered', which uses 'fillReadBuffer'.
defaultReadBuffered :: BufferedIO dev
                    => dev -> Buffer Word8 -> Ptr Word8 -> Int
                    -> IO (Int, Buffer Word8)
defaultReadBuffered dev buf ptr count
  | isEmptyBuffer buf = do
      (r, buf') <- fillReadBuffer dev buf
      if r <= 0
         then return (0, buf')
         else copyFromBuffer buf' ptr count
  | otherwise =
      copyFromBuffer buf ptr count
      -- We could, in theory, do a nonblocking read to get even more bytes.
      -- We don't because 'fillReadBuffer0' might block, even though it's
      -- not supposed to.  See #5843.

-- | Default implementation of 'readBuffered0', which uses 'fillReadBuffer0'.
defaultReadBuffered0 :: BufferedIO dev
                     => dev -> Buffer Word8 -> Ptr Word8 -> Int
                     -> IO (Maybe Int, Buffer Word8)
defaultReadBuffered0 dev buf ptr count
  | isEmptyBuffer buf = do
      (r, buf') <- fillReadBuffer0 dev buf
      case r of
        Nothing            -> return (Nothing, buf')
        Just n | n <= 0    -> return (Just 0, buf')
               | otherwise -> fstJust $ copyFromBuffer buf' ptr count
  | otherwise =
      fstJust $ copyFromBuffer buf ptr count
  where
    fstJust = fmap (\(a, b) -> (Just a, b))

-- | Implements 'readBuffered' by doing large reads directly from the device.
rawIOReadBuffered :: (BufferedIO dev, RawIO dev)
                  => dev -> Buffer Word8 -> Ptr Word8 -> Int
                  -> IO (Int, Buffer Word8)
rawIOReadBuffered dev buf@Buffer{ bufSize } ptr count
  | isEmptyBuffer buf && count >= bufSize =
      fmap (\r -> (r, buf)) $ RawIO.read dev ptr count
  | otherwise =
      defaultReadBuffered dev buf ptr count

-- | Implements 'readBuffered0' by doing large reads directly from the device.
rawIOReadBuffered0 :: (BufferedIO dev, RawIO dev)
                   => dev -> Buffer Word8 -> Ptr Word8 -> Int
                   -> IO (Maybe Int, Buffer Word8)
rawIOReadBuffered0 dev buf@Buffer{ bufSize } ptr count
  | isEmptyBuffer buf && count >= bufSize =
      fmap (\r -> (r, buf)) $ RawIO.readNonBlocking dev ptr count
  | otherwise =
      defaultReadBuffered0 dev buf ptr count

-- ---------------------------------------------------------------------------
-- writeBuffered

-- | Default implementation of 'writeBuffered', which uses 'flushWriteBuffer'.
defaultWriteBuffered :: BufferedIO dev
                     => dev -> Bool -> Buffer Word8 -> Ptr Word8 -> Int
                     -> IO (Buffer Word8)
defaultWriteBuffered dev flushAfter buf0 ptr0 count0 =
    go buf0 ptr0 count0
  where
    go buf ptr count = do
        (r, buf') <- copyToBuffer buf ptr count
        let !ptr'   = ptr `plusPtr` r
            !count' = count - r
        if count' <= 0
           then finish buf'
           else do
                buf'' <- flushWriteBuffer dev buf'
                go buf'' ptr' count'

    finish buf
      | flushAfter || isFullBuffer buf = flushWriteBuffer dev buf
      | otherwise = return buf

-- | Default implementation of 'writeBuffered0', which uses
-- 'flushWriteBuffer' and 'flushWriteBuffer0'.
defaultWriteBuffered0 :: BufferedIO dev
                      => dev -> Bool -> Buffer Word8 -> Ptr Word8 -> Int
                      -> IO (Int, Buffer Word8)
defaultWriteBuffered0 dev flushAfter buf ptr count
  | flushAfter =
      flushIfNotEmpty0 dev buf $ \buf' -> do
        -- Copy bytes to the buffer and do a blocking flush.
        --
        -- The reason we can't simply do a non-blocking flush is that, if the
        -- flush does not complete, we will still have some of our data sitting
        -- in the buffer.  I suppose we could retract these bytes by changing
        -- bufR, but that would make brittle assumptions about the device and
        -- the flushWriteBuffer0 implementation.
        (r, buf'') <- copyToBuffer buf' ptr count
        buf''' <- flushWriteBuffer dev buf''
        return (r, buf''')

  | otherwise = do
      -- Write as many bytes as we can into the buffer without filling it
      -- completely.  If we can't write any right now, flush and try again.
      (r, buf') <- copyToBuffer0 buf ptr count
      if r > 0
         then return (r, buf')
         else do
              (_, buf'') <- flushWriteBuffer0 dev buf'
              copyToBuffer0 buf'' ptr count

-- | Implements 'writeBuffered' by writing directly to the device,
-- except for small writes in 'System.IO.BlockBuffering' mode.
rawIOWriteBuffered :: (BufferedIO dev, RawIO dev)
                   => dev -> Bool -> Buffer Word8 -> Ptr Word8 -> Int
                   -> IO (Buffer Word8)
rawIOWriteBuffered dev flushAfter buf@Buffer{ bufSize } ptr count
  | flushAfter || count >= bufSize =
      flushIfNotEmpty dev buf $ \buf' -> do
        RawIO.write dev ptr count
        return buf'
  | otherwise =
      defaultWriteBuffered dev flushAfter buf ptr count

-- | Implements 'writeBuffered0' by writing directly to the device,
-- except for small writes in 'System.IO.BlockBuffering' mode.
-- Unlike the default 'writeBuffered0', this does not do a blocking flush.
rawIOWriteBuffered0 :: (BufferedIO dev, RawIO dev)
                    => dev -> Bool -> Buffer Word8 -> Ptr Word8 -> Int
                    -> IO (Int, Buffer Word8)
rawIOWriteBuffered0 dev flushAfter buf@Buffer{ bufSize } ptr count
  | flushAfter || count >= bufSize =
      flushIfNotEmpty0 dev buf $ \buf' -> do
        r <- RawIO.writeNonBlocking dev ptr count
        return (r, buf')
  | otherwise =
      defaultWriteBuffered0 dev flushAfter buf ptr count

-- | Flush the buffer if it is not empty, and pass the emptied buffer
-- to the continuation.
flushIfNotEmpty :: BufferedIO dev
                => dev -> Buffer Word8 -> (Buffer Word8 -> IO a) -> IO a
flushIfNotEmpty dev buf cont
  | isEmptyBuffer buf = cont buf
  | otherwise         = flushWriteBuffer dev buf >>= cont

-- | Try to flush the buffer if it is not empty by doing a non-blocking flush.
-- If successful, call the continuation with the emptied buffer.
-- Otherwise, return 0.
flushIfNotEmpty0 :: BufferedIO dev
                 => dev
                 -> Buffer Word8
                 -> (Buffer Word8 -> IO (Int, Buffer Word8))
                 -> IO (Int, Buffer Word8)
flushIfNotEmpty0 dev buf cont
  | isEmptyBuffer buf = cont buf
  | otherwise = do
      (_, buf') <- flushWriteBuffer0 dev buf
      if isEmptyBuffer buf'
         then cont buf'
         else return (0, buf')

-- ---------------------------------------------------------------------------
-- memcpy wrappers

-- | Copy bytes from the 'Buffer'.  Stop when the request is satisfied or the
-- 'Buffer' runs out of data, whichever comes first.
copyFromBuffer :: Buffer Word8 -> Ptr Word8 -> Int -> IO (Int, Buffer Word8)
copyFromBuffer buf@Buffer{ bufRaw, bufL } ptr count = do
    let !count' = min count (bufferElems buf)
        !buf'   = bufferRemove count' buf
    withRawBuffer bufRaw $ \praw -> do
        _ <- memcpy ptr (praw `plusPtr` bufL) (fromIntegral count')
        return ()
    return (count', buf')

-- | Copy bytes to the 'Buffer'.  Stop when all of the bytes are written or the
-- 'Buffer' runs out of space at the end, whichever comes first.
--
-- This may return a full buffer.  One of the Buffer invariants is that
-- "a write buffer is never full" (see GHC.IO.Buffer).  Be sure to flush
-- any full Buffer before returning it to the wild.
--
-- This does not slide bytes to the left to make room for the new data.
-- It simply starts at 'bufR'.
copyToBuffer :: Buffer Word8 -> Ptr Word8 -> Int -> IO (Int, Buffer Word8)
copyToBuffer buf@Buffer{ bufRaw, bufR } ptr count = do
    let !count' = min count (bufferAvailable buf)
        !buf'   = bufferAdd count' buf
    withRawBuffer bufRaw $ \praw -> do
        _ <- memcpy (praw `plusPtr` bufR) ptr (fromIntegral count')
        return ()
    return (count', buf')

-- | Variant of 'copyToBuffer' that will not fill the buffer completely.
-- This is used to preserve the "write buffer is never full" Buffer invariant
-- in non-blocking writes.
copyToBuffer0 :: Buffer Word8 -> Ptr Word8 -> Int -> IO (Int, Buffer Word8)
copyToBuffer0 buf ptr count
  | avail < 1 = return (0, buf)
  | otherwise = copyToBuffer buf ptr (min count avail)
  where
    avail = bufferAvailable buf - 1

foreign import ccall unsafe "memcpy"
    memcpy :: Ptr a -> Ptr a -> CSize -> IO (Ptr a)
