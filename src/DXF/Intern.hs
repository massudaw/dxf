{-# LANGUAGE BangPatterns #-}
-- | String interning: a document names a few distinct layers, linetypes,
-- styles and blocks on thousands of entities.  An 'Interner' maps the
-- bytes of such a string to the one copy that every entity then holds, so
-- that none keeps a slice of the input alive and equal strings are the
-- same bytes in memory ('==' on 'B.ByteString' tries the pointer first).
--
-- One table is made per parse and used by the fast decoders and by the
-- general path alike.  It is a direct-mapped cache in front of a map:
-- the hot case, the same string as a moment ago, is a hash of a few
-- bytes and one comparison.  'intern' is pure -- interning does not change
-- a value, so the order in which strings reach the table does not matter.
module DXF.Intern
  ( Interner, newInterner, intern, internMaybe, internSize
  ) where

import Control.Monad.ST (RealWorld, stToIO)
import Data.Array.IO (IOArray, newArray, readArray, writeArray)
import Data.Bits (shiftR, xor)
import qualified Data.ByteString as B
import Data.IORef
import qualified Data.Map.Strict as M
import LinBuf.Ints (Ints, newInts, readMasked, writeMasked)
import System.IO.Unsafe (unsafeDupablePerformIO)

-- | The cache holds, for each hash slot, the number of the string last seen
-- there (-1: none); the strings are kept in 'iStrings' by number, and the
-- map gives the number of every string held.
data Interner = Interner
  { iCache   :: !(Ints RealWorld)
  , iStrings :: !(IOArray Int B.ByteString)
  , iMap     :: !(IORef (M.Map B.ByteString Int))
  }

-- | A new table.  Strings longer than 'maxLen' are left alone, and the
-- table stops growing at 'maxStrings'.
newInterner :: IO Interner
newInterner = Interner <$> stToIO (newInts slots (-1)) <*> newArray (0, maxStrings - 1) B.empty <*> newIORef M.empty

slots, maxLen, maxStrings :: Int
slots = 1024
maxLen = 64
maxStrings = 20000

-- | The distinct strings held.
internSize :: Interner -> IO Int
internSize t = M.size <$> readIORef (iMap t)

hash :: B.ByteString -> Int
hash v = let h = B.foldl' (\a w -> a * 33 + fromIntegral w) 5381 v in h `xor` (h `shiftR` 10)

-- | The one copy of a string.
intern :: Interner -> B.ByteString -> B.ByteString
intern t v
  | n == 0 || n > maxLen = v
  | otherwise = unsafeDupablePerformIO $ do
      let !h = hash v
      k <- stToIO (readMasked (iCache t) h)
      if k < 0 then slow t h v else do
        c <- readArray (iStrings t) k
        if c == v then pure c else slow t h v
  where n = B.length v
{-# INLINE intern #-}

slow :: Interner -> Int -> B.ByteString -> IO B.ByteString
slow t h v = do
  m <- readIORef (iMap t)
  case M.lookup v m of
    Just k -> stToIO (writeMasked (iCache t) h k) >> readArray (iStrings t) k
    Nothing
      | M.size m >= maxStrings -> pure v
      | otherwise -> do
          let !c = B.copy v
              !k = M.size m
          writeArray (iStrings t) k c
          writeIORef (iMap t) $! M.insert c k m
          stToIO (writeMasked (iCache t) h k)
          pure c
{-# NOINLINE slow #-}

internMaybe :: Interner -> Maybe B.ByteString -> Maybe B.ByteString
internMaybe t (Just v) = Just $! intern t v
internMaybe _ Nothing = Nothing
{-# INLINE internMaybe #-}
