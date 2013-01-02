{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP
           , NoImplicitPrelude
           , RecordWildCards
           , BangPatterns
           , PatternGuards
           , NondecreasingIndentation
           , MagicHash
           , ForeignFunctionInterface
  #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IO.Text
-- Copyright   :  (c) The University of Glasgow, 1992-2008
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- String I\/O functions
--
-----------------------------------------------------------------------------

-- #hide
module GHC.IO.Handle.Text ( 
        hWaitForInput, hGetChar, hGetLine, hGetContents, hPutChar, hPutStr,
        commitBuffer',       -- hack, see below
        hGetBuf, hGetBufSome, hGetBufNonBlocking, hPutBuf, hPutBufNonBlocking,
        memcpy, hPutStrLn,
    ) where

import GHC.IO
import GHC.IO.Buffer
import GHC.IO.BufferedIO (BufferedIO)
import qualified GHC.IO.BufferedIO as BufferedIO
import GHC.IO.Exception
import GHC.Exception
import GHC.IO.Handle.Types
import GHC.IO.Handle.Internals
import qualified GHC.IO.Device as IODevice

import Foreign
import Foreign.C

import qualified Control.Exception as Exception
import System.IO.Error
import Data.Maybe
import Control.Monad

import GHC.IORef
import GHC.Base
import GHC.Num
import GHC.Show
import GHC.List

-- ---------------------------------------------------------------------------
-- Simple input operations

-- If hWaitForInput finds anything in the Handle's buffer, it
-- immediately returns.  If not, it tries to read from the underlying
-- OS handle. Notice that for buffered Handles connected to terminals
-- this means waiting until a complete line is available.

-- | Computation 'hWaitForInput' @hdl t@
-- waits until input is available on handle @hdl@.
-- It returns 'True' as soon as input is available on @hdl@,
-- or 'False' if no input is available within @t@ milliseconds.  Note that
-- 'hWaitForInput' waits until one or more full /characters/ are available,
-- which means that it needs to do decoding, and hence may fail
-- with a decoding error.
--
-- If @t@ is less than zero, then @hWaitForInput@ waits indefinitely.
--
-- This operation may fail with:
--
--  * 'isEOFError' if the end of file has been reached.
--
--  * a decoding error, if the input begins with an invalid byte sequence
--    in this Handle's encoding.
--
-- NOTE for GHC users: unless you use the @-threaded@ flag,
-- @hWaitForInput t@ where @t >= 0@ will block all other Haskell
-- threads for the duration of the call.  It behaves like a
-- @safe@ foreign call in this respect.
--

hWaitForInput :: Handle -> Int -> IO Bool
hWaitForInput h msecs = do
  wantReadableHandle_ "hWaitForInput" h $ \ handle_@Handle__{..} -> do
  cbuf <- readIORef haCharBuffer

  if not (isEmptyBuffer cbuf) then return True else do

  if msecs < 0 
        then do cbuf' <- readTextDevice handle_ cbuf
                writeIORef haCharBuffer cbuf'
                return True
        else do
               -- there might be bytes in the byte buffer waiting to be decoded
               cbuf' <- decodeByteBuf handle_ cbuf
               writeIORef haCharBuffer cbuf'

               if not (isEmptyBuffer cbuf') then return True else do

                r <- IODevice.ready haDevice False{-read-} msecs
                if r then do -- Call hLookAhead' to throw an EOF
                             -- exception if appropriate
                             _ <- hLookAhead_ handle_
                             return True
                     else return False
                -- XXX we should only return when there are full characters
                -- not when there are only bytes.  That would mean looping
                -- and re-running IODevice.ready if we don't have any full
                -- characters; but we don't know how long we've waited
                -- so far.

-- ---------------------------------------------------------------------------
-- hGetChar

-- | Computation 'hGetChar' @hdl@ reads a character from the file or
-- channel managed by @hdl@, blocking until a character is available.
--
-- This operation may fail with:
--
--  * 'isEOFError' if the end of file has been reached.

hGetChar :: Handle -> IO Char
hGetChar handle =
  wantReadableHandle_ "hGetChar" handle $ \handle_@Handle__{..} -> do

  -- buffering mode makes no difference: we just read whatever is available
  -- from the device (blocking only if there is nothing available), and then
  -- return the first character.
  -- See [note Buffered Reading] in GHC.IO.Handle.Types
  buf0 <- readIORef haCharBuffer

  buf1 <- if isEmptyBuffer buf0
             then readTextDevice handle_ buf0
             else return buf0

  (c1,i) <- readCharBuf (bufRaw buf1) (bufL buf1)
  let buf2 = bufferAdjustL i buf1

  if haInputNL == CRLF && c1 == '\r'
     then do
            mbuf3 <- if isEmptyBuffer buf2
                      then maybeFillReadBuffer handle_ buf2
                      else return (Just buf2)

            case mbuf3 of
               -- EOF, so just return the '\r' we have
               Nothing -> do
                  writeIORef haCharBuffer buf2
                  return '\r'
               Just buf3 -> do
                  (c2,i2) <- readCharBuf (bufRaw buf2) (bufL buf2)
                  if c2 == '\n'
                     then do
                       writeIORef haCharBuffer (bufferAdjustL i2 buf3)
                       return '\n'
                     else do
                       -- not a \r\n sequence, so just return the \r
                       writeIORef haCharBuffer buf3
                       return '\r'
     else do
            writeIORef haCharBuffer buf2
            return c1

-- ---------------------------------------------------------------------------
-- hGetLine

-- | Computation 'hGetLine' @hdl@ reads a line from the file or
-- channel managed by @hdl@.
--
-- This operation may fail with:
--
--  * 'isEOFError' if the end of file is encountered when reading
--    the /first/ character of the line.
--
-- If 'hGetLine' encounters end-of-file at any other point while reading
-- in a line, it is treated as a line terminator and the (partial)
-- line is returned.

hGetLine :: Handle -> IO String
hGetLine h =
  wantReadableHandle_ "hGetLine" h $ \ handle_ -> do
     hGetLineBuffered handle_

hGetLineBuffered :: Handle__ -> IO String
hGetLineBuffered handle_@Handle__{..} = do
  buf <- readIORef haCharBuffer
  hGetLineBufferedLoop handle_ buf []

hGetLineBufferedLoop :: Handle__
                     -> CharBuffer -> [String]
                     -> IO String
hGetLineBufferedLoop handle_@Handle__{..}
        buf@Buffer{ bufL=r0, bufR=w, bufRaw=raw0 } xss =
  let
        -- find the end-of-line character, if there is one
        loop raw r
           | r == w = return (False, w)
           | otherwise =  do
                (c,r') <- readCharBuf raw r
                if c == '\n'
                   then return (True, r) -- NB. not r': don't include the '\n'
                   else loop raw r'
  in do
  (eol, off) <- loop raw0 r0

  debugIO ("hGetLineBufferedLoop: r=" ++ show r0 ++ ", w=" ++ show w ++ ", off=" ++ show off)

  (xs,r') <- if haInputNL == CRLF
                then unpack_nl raw0 r0 off ""
                else do xs <- unpack raw0 r0 off ""
                        return (xs,off)

  -- if eol == True, then off is the offset of the '\n'
  -- otherwise off == w and the buffer is now empty.
  if eol -- r' == off
        then do writeIORef haCharBuffer (bufferAdjustL (off+1) buf)
                return (concat (reverse (xs:xss)))
        else do
             let buf1 = bufferAdjustL r' buf
             maybe_buf <- maybeFillReadBuffer handle_ buf1
             case maybe_buf of
                -- Nothing indicates we caught an EOF, and we may have a
                -- partial line to return.
                Nothing -> do
                     -- we reached EOF.  There might be a lone \r left
                     -- in the buffer, so check for that and
                     -- append it to the line if necessary.
                     -- 
                     let pre = if not (isEmptyBuffer buf1) then "\r" else ""
                     writeIORef haCharBuffer buf1{ bufL=0, bufR=0 }
                     let str = concat (reverse (pre:xs:xss))
                     if not (null str)
                        then return str
                        else ioe_EOF
                Just new_buf ->
                     hGetLineBufferedLoop handle_ new_buf (xs:xss)

maybeFillReadBuffer :: Handle__ -> CharBuffer -> IO (Maybe CharBuffer)
maybeFillReadBuffer handle_ buf
  = Exception.catch
     (do buf' <- getSomeCharacters handle_ buf
         return (Just buf')
     )
     (\e -> do if isEOFError e
                  then return Nothing
                  else ioError e)

-- See GHC.IO.Buffer
#define CHARBUF_UTF32
-- #define CHARBUF_UTF16

-- NB. performance-critical code: eyeball the Core.
unpack :: RawCharBuffer -> Int -> Int -> [Char] -> IO [Char]
unpack !buf !r !w acc0
 | r == w    = return acc0
 | otherwise = 
  withRawBuffer buf $ \pbuf -> 
    let
        unpackRB acc !i
         | i < r  = return acc
         | otherwise = do
              -- Here, we are rather careful to only put an *evaluated* character
              -- in the output string. Due to pointer tagging, this allows the consumer
              -- to avoid ping-ponging between the actual consumer code and the thunk code
#ifdef CHARBUF_UTF16
              -- reverse-order decoding of UTF-16
              c2 <- peekElemOff pbuf i
              if (c2 < 0xdc00 || c2 > 0xdffff)
                 then unpackRB (unsafeChr (fromIntegral c2) : acc) (i-1)
                 else do c1 <- peekElemOff pbuf (i-1)
                         let c = (fromIntegral c1 - 0xd800) * 0x400 +
                                 (fromIntegral c2 - 0xdc00) + 0x10000
                         case desurrogatifyRoundtripCharacter (unsafeChr c) of
                           { C# c# -> unpackRB (C# c# : acc) (i-2) }
#else
              c <- peekElemOff pbuf i
              unpackRB (c : acc) (i-1)
#endif
     in
     unpackRB acc0 (w-1)

-- NB. performance-critical code: eyeball the Core.
unpack_nl :: RawCharBuffer -> Int -> Int -> [Char] -> IO ([Char],Int)
unpack_nl !buf !r !w acc0
 | r == w    =  return (acc0, 0)
 | otherwise =
  withRawBuffer buf $ \pbuf ->
    let
        unpackRB acc !i
         | i < r  = return acc
         | otherwise = do
              c <- peekElemOff pbuf i
              if (c == '\n' && i > r)
                 then do
                         c1 <- peekElemOff pbuf (i-1)
                         if (c1 == '\r')
                            then unpackRB ('\n':acc) (i-2)
                            else unpackRB ('\n':acc) (i-1)
                 else do
                         unpackRB (c : acc) (i-1)
     in do
     c <- peekElemOff pbuf (w-1)
     if (c == '\r')
        then do 
                -- If the last char is a '\r', we need to know whether or
                -- not it is followed by a '\n', so leave it in the buffer
                -- for now and just unpack the rest.
                str <- unpackRB acc0 (w-2)
                return (str, w-1)
        else do
                str <- unpackRB acc0 (w-1)
                return (str, w)

-- Note [#5536]
--
-- We originally had
--
--    let c' = desurrogatifyRoundtripCharacter c in
--    c' `seq` unpackRB (c':acc) (i-1)
--
-- but this resulted in Core like
--
--    case (case x <# y of True -> C# e1; False -> C# e2) of c
--      C# _ -> unpackRB (c:acc) (i-1)
--
-- which compiles into a continuation for the outer case, with each
-- branch of the inner case building a C# and then jumping to the
-- continuation.  We'd rather not have this extra jump, which makes
-- quite a difference to performance (see #5536) It turns out that
-- matching on the C# directly causes GHC to do the case-of-case,
-- giving much straighter code.

-- -----------------------------------------------------------------------------
-- hGetContents

-- hGetContents on a DuplexHandle only affects the read side: you can
-- carry on writing to it afterwards.

-- | Computation 'hGetContents' @hdl@ returns the list of characters
-- corresponding to the unread portion of the channel or file managed
-- by @hdl@, which is put into an intermediate state, /semi-closed/.
-- In this state, @hdl@ is effectively closed,
-- but items are read from @hdl@ on demand and accumulated in a special
-- list returned by 'hGetContents' @hdl@.
--
-- Any operation that fails because a handle is closed,
-- also fails if a handle is semi-closed.  The only exception is 'hClose'.
-- A semi-closed handle becomes closed:
--
--  * if 'hClose' is applied to it;
--
--  * if an I\/O error occurs when reading an item from the handle;
--
--  * or once the entire contents of the handle has been read.
--
-- Once a semi-closed handle becomes closed, the contents of the
-- associated list becomes fixed.  The contents of this final list is
-- only partially specified: it will contain at least all the items of
-- the stream that were evaluated prior to the handle becoming closed.
--
-- Any I\/O errors encountered while a handle is semi-closed are simply
-- discarded.
--
-- This operation may fail with:
--
--  * 'isEOFError' if the end of file has been reached.

hGetContents :: Handle -> IO String
hGetContents handle = 
   wantReadableHandle "hGetContents" handle $ \handle_ -> do
      xs <- lazyRead handle
      return (handle_{ haType=SemiClosedHandle}, xs )

-- Note that someone may close the semi-closed handle (or change its
-- buffering), so each time these lazy read functions are pulled on,
-- they have to check whether the handle has indeed been closed.

lazyRead :: Handle -> IO String
lazyRead handle = 
   unsafeInterleaveIO $
        withHandle "hGetContents" handle $ \ handle_ -> do
        case haType handle_ of
          ClosedHandle     -> return (handle_, "")
          SemiClosedHandle -> lazyReadBuffered handle handle_
          _ -> ioException 
                  (IOError (Just handle) IllegalOperation "hGetContents"
                        "illegal handle type" Nothing Nothing)

lazyReadBuffered :: Handle -> Handle__ -> IO (Handle__, [Char])
lazyReadBuffered h handle_@Handle__{..} = do
   buf <- readIORef haCharBuffer
   Exception.catch
        (do
            buf'@Buffer{..} <- getSomeCharacters handle_ buf
            lazy_rest <- lazyRead h
            (s,r) <- if haInputNL == CRLF
                         then unpack_nl bufRaw bufL bufR lazy_rest
                         else do s <- unpack bufRaw bufL bufR lazy_rest
                                 return (s,bufR)
            writeIORef haCharBuffer (bufferAdjustL r buf')
            return (handle_, s)
        )
        (\e -> do (handle_', _) <- hClose_help handle_
                  debugIO ("hGetContents caught: " ++ show e)
                  -- We might have a \r cached in CRLF mode.  So we
                  -- need to check for that and return it:
                  let r = if isEOFError e
                             then if not (isEmptyBuffer buf)
                                     then "\r"
                                     else ""
                             else
                                  throw (augmentIOError e "hGetContents" h)

                  return (handle_', r)
        )

-- ensure we have some characters in the buffer
getSomeCharacters :: Handle__ -> CharBuffer -> IO CharBuffer
getSomeCharacters handle_@Handle__{..} buf@Buffer{..} =
  case bufferElems buf of

    -- buffer empty: read some more
    0 -> readTextDevice handle_ buf

    -- if the buffer has a single '\r' in it and we're doing newline
    -- translation: read some more
    1 | haInputNL == CRLF -> do
      (c,_) <- readCharBuf bufRaw bufL
      if c == '\r'
         then do -- shuffle the '\r' to the beginning.  This is only safe
                 -- if we're about to call readTextDevice, otherwise it
                 -- would mess up flushCharBuffer.
                 -- See [note Buffer Flushing], GHC.IO.Handle.Types
                 _ <- writeCharBuf bufRaw 0 '\r'
                 let buf' = buf{ bufL=0, bufR=1 }
                 readTextDevice handle_ buf'
         else do
                 return buf

    -- buffer has some chars in it already: just return it
    _otherwise ->
      return buf

-- ---------------------------------------------------------------------------
-- hPutChar

-- | Computation 'hPutChar' @hdl ch@ writes the character @ch@ to the
-- file or channel managed by @hdl@.  Characters may be buffered if
-- buffering is enabled for @hdl@.
--
-- This operation may fail with:
--
--  * 'isFullError' if the device is full; or
--
--  * 'isPermissionError' if another system resource limit would be exceeded.

hPutChar :: Handle -> Char -> IO ()
hPutChar handle c = do
    c `seq` return ()
    wantWritableHandle "hPutChar" handle $ \ handle_  -> do
     hPutcBuffered handle_ c

hPutcBuffered :: Handle__ -> Char -> IO ()
hPutcBuffered handle_@Handle__{..} c = do
  buf <- readIORef haCharBuffer
  if c == '\n'
     then do buf1 <- if haOutputNL == CRLF
                        then do
                          buf1 <- putc buf '\r'
                          putc buf1 '\n'
                        else do
                          putc buf '\n'
             writeCharBuffer handle_ buf1
             when is_line $ flushByteWriteBuffer handle_
      else do
          buf1 <- putc buf c
          writeCharBuffer handle_ buf1
          return ()
  where
    is_line = case haBufferMode of
                LineBuffering -> True
                _             -> False

    putc buf@Buffer{ bufRaw=raw, bufR=w } c = do
       debugIO ("putc: " ++ summaryBuffer buf)
       w'  <- writeCharBuf raw w c
       return buf{ bufR = w' }

-- ---------------------------------------------------------------------------
-- hPutStr

-- We go to some trouble to avoid keeping the handle locked while we're
-- evaluating the string argument to hPutStr, in case doing so triggers another
-- I/O operation on the same handle which would lead to deadlock.  The classic
-- case is
--
--              putStr (trace "hello" "world")
--
-- so the basic scheme is this:
--
--      * copy the string into a fresh buffer,
--      * "commit" the buffer to the handle.
--
-- Committing may involve simply copying the contents of the new
-- buffer into the handle's buffer, flushing one or both buffers, or
-- maybe just swapping the buffers over (if the handle's buffer was
-- empty).  See commitBuffer below.

-- | Computation 'hPutStr' @hdl s@ writes the string
-- @s@ to the file or channel managed by @hdl@.
--
-- This operation may fail with:
--
--  * 'isFullError' if the device is full; or
--
--  * 'isPermissionError' if another system resource limit would be exceeded.

hPutStr :: Handle -> String -> IO ()
hPutStr handle str = hPutStr' handle str False

-- | The same as 'hPutStr', but adds a newline character.
hPutStrLn :: Handle -> String -> IO ()
hPutStrLn handle str = hPutStr' handle str True
  -- An optimisation: we treat hPutStrLn specially, to avoid the
  -- overhead of a single putChar '\n', which is quite high now that we
  -- have to encode eagerly.

hPutStr' :: Handle -> String -> Bool -> IO ()
hPutStr' handle str add_nl =
  do
    (buffer_mode, nl) <-
         wantWritableHandle "hPutStr" handle $ \h_ -> do
                       bmode <- getSpareBuffer h_
                       return (bmode, haOutputNL h_)

    case buffer_mode of
       (NoBuffering, _) -> do
            hPutChars handle str        -- v. slow, but we don't care
            when add_nl $ hPutChar handle '\n'
       (LineBuffering, buf) -> do
            writeBlocks handle True  add_nl nl buf str
       (BlockBuffering _, buf) -> do
            writeBlocks handle False add_nl nl buf str

hPutChars :: Handle -> [Char] -> IO ()
hPutChars _      [] = return ()
hPutChars handle (c:cs) = hPutChar handle c >> hPutChars handle cs

getSpareBuffer :: Handle__ -> IO (BufferMode, CharBuffer)
getSpareBuffer Handle__{haCharBuffer=ref, 
                        haBuffers=spare_ref,
                        haBufferMode=mode}
 = do
   case mode of
     NoBuffering -> return (mode, error "no buffer!")
     _ -> do
          bufs <- readIORef spare_ref
          buf  <- readIORef ref
          case bufs of
            BufferListCons b rest -> do
                writeIORef spare_ref rest
                return ( mode, emptyBuffer b (bufSize buf) WriteBuffer)
            BufferListNil -> do
                new_buf <- newCharBuffer (bufSize buf) WriteBuffer
                return (mode, new_buf)


-- NB. performance-critical code: eyeball the Core.
writeBlocks :: Handle -> Bool -> Bool -> Newline -> Buffer CharBufElem -> String -> IO ()
writeBlocks hdl line_buffered add_nl nl
            buf@Buffer{ bufRaw=raw, bufSize=len } s =
  let
   shoveString :: Int -> [Char] -> [Char] -> IO ()
   shoveString !n [] [] = do
        commitBuffer hdl raw len n False{-no flush-} True{-release-}
   shoveString !n [] rest = do
        shoveString n rest []
   shoveString !n (c:cs) rest
     -- n+1 so we have enough room to write '\r\n' if necessary
     | n + 1 >= len = do
        commitBuffer hdl raw len n False{-flush-} False
        shoveString 0 (c:cs) rest
     | c == '\n'  =  do
        n' <- if nl == CRLF
                 then do 
                    n1 <- writeCharBuf raw n  '\r'
                    writeCharBuf raw n1 '\n'
                 else do
                    writeCharBuf raw n c
        if line_buffered
           then do
                -- end of line, so write and flush
               commitBuffer hdl raw len n' True{-flush-} False
               shoveString 0 cs rest
           else do
               shoveString n' cs rest
     | otherwise = do
        n' <- writeCharBuf raw n c
        shoveString n' cs rest
  in
  shoveString 0 s (if add_nl then "\n" else "")

-- -----------------------------------------------------------------------------
-- commitBuffer handle buf sz count flush release
-- 
-- Write the contents of the buffer 'buf' ('sz' bytes long, containing
-- 'count' bytes of data) to handle (handle must be block or line buffered).

commitBuffer
        :: Handle                       -- handle to commit to
        -> RawCharBuffer -> Int         -- address and size (in bytes) of buffer
        -> Int                          -- number of bytes of data in buffer
        -> Bool                         -- True <=> flush the handle afterward
        -> Bool                         -- release the buffer?
        -> IO ()

commitBuffer hdl !raw !sz !count flush release = 
  wantWritableHandle "commitBuffer" hdl $ \h_@Handle__{..} -> do
      debugIO ("commitBuffer: sz=" ++ show sz ++ ", count=" ++ show count
            ++ ", flush=" ++ show flush ++ ", release=" ++ show release)

      writeCharBuffer h_ Buffer{ bufRaw=raw, bufState=WriteBuffer,
                                 bufL=0, bufR=count, bufSize=sz }

      when flush $ flushByteWriteBuffer h_

      -- release the buffer if necessary
      when release $ do
          -- find size of current buffer
          old_buf@Buffer{ bufSize=size } <- readIORef haCharBuffer
          when (sz == size) $ do
               spare_bufs <- readIORef haBuffers
               writeIORef haBuffers (BufferListCons raw spare_bufs)

      return ()

-- backwards compatibility; the text package uses this
commitBuffer' :: RawCharBuffer -> Int -> Int -> Bool -> Bool -> Handle__
              -> IO CharBuffer
commitBuffer' raw sz@(I# _) count@(I# _) flush release h_@Handle__{..}
   = do
      debugIO ("commitBuffer: sz=" ++ show sz ++ ", count=" ++ show count
            ++ ", flush=" ++ show flush ++ ", release=" ++ show release)

      let this_buf = Buffer{ bufRaw=raw, bufState=WriteBuffer,
                             bufL=0, bufR=count, bufSize=sz }

      writeCharBuffer h_ this_buf

      when flush $ flushByteWriteBuffer h_

      -- release the buffer if necessary
      when release $ do
          -- find size of current buffer
          old_buf@Buffer{ bufSize=size } <- readIORef haCharBuffer
          when (sz == size) $ do
               spare_bufs <- readIORef haBuffers
               writeIORef haBuffers (BufferListCons raw spare_bufs)

      return this_buf

-- ---------------------------------------------------------------------------
-- Reading/writing sequences of bytes.

-- ---------------------------------------------------------------------------
-- hPutBuf

-- | 'hPutBuf' @hdl buf count@ writes @count@ 8-bit bytes from the
-- buffer @buf@ to the handle @hdl@.  It returns ().
--
-- 'hPutBuf' ignores any text encoding that applies to the 'Handle',
-- writing the bytes directly to the underlying file or device.
--
-- 'hPutBuf' ignores the prevailing 'TextEncoding' and
-- 'NewlineMode' on the 'Handle', and writes bytes directly.
--
-- This operation may fail with:
--
--  * 'ResourceVanished' if the handle is a pipe or socket, and the
--    reading end is closed.  (If this is a POSIX system, and the program
--    has not asked to ignore SIGPIPE, then a SIGPIPE may be delivered
--    instead, whose default action is to terminate the program).

hPutBuf :: Handle                       -- handle to write to
        -> Ptr a                        -- address of buffer
        -> Int                          -- number of bytes of data in buffer
        -> IO ()
hPutBuf handle ptr count
  | count == 0 = return ()
  | count <  0 = illegalBufferSize handle "hPutBuf" count
  | otherwise =
    wantWritableHandle "hPutBuf" handle $
      \Handle__{..} -> do
          debugIO ("hPutBuf count=" ++ show count)
          let needFlush = bufferModeNeedsFlush haBufferMode
          buf <- readIORef haByteBuffer
          !buf' <- BufferedIO.writeBuffered haDevice needFlush buf
                                            (castPtr ptr) count
          writeIORef haByteBuffer buf'

-- | Non-blocking version of 'hPutBuf'.  Returns the number of bytes written,
-- which may be a short count or zero.
hPutBufNonBlocking
        :: Handle                       -- handle to write to
        -> Ptr a                        -- address of buffer
        -> Int                          -- number of bytes of data in buffer
        -> IO Int                       -- returns: number of bytes written
hPutBufNonBlocking handle ptr count
  | count == 0 = return 0
  | count <  0 = illegalBufferSize handle "hPutBufNonBlocking" count
  | otherwise =
    wantWritableHandle "hPutBufNonBlocking" handle $
      \Handle__{..} -> do
          debugIO ("hPutBufNonBlocking count=" ++ show count)
          let needFlush = bufferModeNeedsFlush haBufferMode
          buf <- readIORef haByteBuffer
          (!r, !buf') <- BufferedIO.writeBuffered0 haDevice needFlush buf
                                                   (castPtr ptr) count
          writeIORef haByteBuffer buf'
          return r

-- We must flush if the Handle is set to NoBuffering.
-- If it is set to LineBuffering, be conservative and flush
-- anyway (we didn't check for newlines in the data).
bufferModeNeedsFlush :: BufferMode -> Bool
bufferModeNeedsFlush NoBuffering        = True
bufferModeNeedsFlush LineBuffering      = True
bufferModeNeedsFlush (BlockBuffering _) = False

-- ---------------------------------------------------------------------------
-- hGetBuf

-- | 'hGetBuf' @hdl buf count@ reads data from the handle @hdl@
-- into the buffer @buf@ until either EOF is reached or
-- @count@ 8-bit bytes have been read.
-- It returns the number of bytes actually read.  This may be zero if
-- EOF was reached before any data was read (or if @count@ is zero).
--
-- 'hGetBuf' never raises an EOF exception, instead it returns a value
-- smaller than @count@.
--
-- If the handle is a pipe or socket, and the writing end
-- is closed, 'hGetBuf' will behave as if EOF was reached.
--
-- 'hGetBuf' ignores the prevailing 'TextEncoding' and 'NewlineMode'
-- on the 'Handle', and reads bytes directly.

hGetBuf :: Handle -> Ptr a -> Int -> IO Int
hGetBuf h ptr count
  | count == 0 = return 0
  | count <  0 = illegalBufferSize h "hGetBuf" count
  | otherwise = 
      wantReadableHandle_ "hGetBuf" h $ \ h_@Handle__{..} -> do
         flushCharReadBuffer h_
         buf <- readIORef haByteBuffer
         (!r, !buf') <- readAllBuffered haDevice buf (castPtr ptr) count
         writeIORef haByteBuffer buf'
         return r

readAllBuffered :: BufferedIO dev
                => dev -> Buffer Word8 -> Ptr Word8 -> Int
                -> IO (Int, Buffer Word8)
readAllBuffered dev buf0 ptr0 count0 =
    go buf0 ptr0 count0
  where
    go buf ptr count = do
        (!r, !buf') <- BufferedIO.readBuffered dev buf ptr count
        let !ptr'    = ptr `plusPtr` r
            !count'  = count - r
        if count' <= 0 || r <= 0  -- read satisfied or EOF
           then done buf' ptr'
           else go buf' ptr' count'

    done buf ptr = do
        let !r = ptr `minusPtr` ptr0
        return (r, buf)

-- ---------------------------------------------------------------------------
-- hGetBufSome

-- | 'hGetBufSome' @hdl buf count@ reads data from the handle @hdl@
-- into the buffer @buf@.  If there is any data available to read,
-- then 'hGetBufSome' returns it immediately; it only blocks if there
-- is no data to be read.
--
-- It returns the number of bytes actually read.  This may be zero if
-- EOF was reached before any data was read (or if @count@ is zero).
--
-- 'hGetBufSome' never raises an EOF exception, instead it returns a value
-- smaller than @count@.
--
-- If the handle is a pipe or socket, and the writing end
-- is closed, 'hGetBufSome' will behave as if EOF was reached.
--
-- 'hGetBufSome' ignores the prevailing 'TextEncoding' and 'NewlineMode'
-- on the 'Handle', and reads bytes directly.

hGetBufSome :: Handle -> Ptr a -> Int -> IO Int
hGetBufSome h ptr count
  | count == 0 = return 0
  | count <  0 = illegalBufferSize h "hGetBufSome" count
  | otherwise =
      wantReadableHandle_ "hGetBufSome" h $ \ h_@Handle__{..} -> do
         flushCharReadBuffer h_
         buf <- readIORef haByteBuffer
         (!r, !buf') <- BufferedIO.readBuffered haDevice buf (castPtr ptr) count
         writeIORef haByteBuffer buf'
         return r

-- | 'hGetBufNonBlocking' @hdl buf count@ reads data from the handle @hdl@
-- into the buffer @buf@ until either EOF is reached, or
-- @count@ 8-bit bytes have been read, or there is no more data available
-- to read immediately.
--
-- 'hGetBufNonBlocking' is identical to 'hGetBuf', except that it will
-- never block waiting for data to become available, instead it returns
-- only whatever data is available.  To wait for data to arrive before
-- calling 'hGetBufNonBlocking', use 'hWaitForInput'.
--
-- If the handle is a pipe or socket, and the writing end
-- is closed, 'hGetBufNonBlocking' will behave as if EOF was reached.
--
-- 'hGetBufNonBlocking' ignores the prevailing 'TextEncoding' and
-- 'NewlineMode' on the 'Handle', and reads bytes directly.
--
-- NOTE: on Windows, this function does not work correctly for files;
-- it behaves identically to 'hGetBufSome'.

hGetBufNonBlocking :: Handle -> Ptr a -> Int -> IO Int
hGetBufNonBlocking h ptr count
  | count == 0 = return 0
  | count <  0 = illegalBufferSize h "hGetBufNonBlocking" count
  | otherwise = 
      wantReadableHandle_ "hGetBufNonBlocking" h $ \ h_@Handle__{..} -> do
         flushCharReadBuffer h_
         buf <- readIORef haByteBuffer
         (r, !buf') <- BufferedIO.readBuffered0 haDevice buf (castPtr ptr) count
         writeIORef haByteBuffer buf'
         case r of
             Nothing            -> return 0 -- EOF
             Just n | n <= 0    -> return 0 -- No data available right now
                    | otherwise -> return n

-----------------------------------------------------------------------------
-- Internal Utils

illegalBufferSize :: Handle -> String -> Int -> IO a
illegalBufferSize handle fn sz =
        ioException (IOError (Just handle)
                            InvalidArgument  fn
                            ("illegal buffer size " ++ showsPrec 9 sz [])
                            Nothing Nothing)

-- GHC.IO.Handle.Text used to use memcpy, but now it doesn't.
-- Still need to export it, as older versions did.
foreign import ccall unsafe "memcpy"
   memcpy :: Ptr a -> Ptr a -> CSize -> IO (Ptr ())
