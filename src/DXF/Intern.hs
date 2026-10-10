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

import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.Array.IO (IOArray, newArray)
import Data.Bits (shiftR, xor, (.&.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Data.IORef
import qualified Data.Map.Strict as M
import System.IO.Unsafe (unsafeDupablePerformIO)

data Interner = Interner
  { iCache :: !(IOArray Int B.ByteString)
  , iMap   :: !(IORef (M.Map B.ByteString B.ByteString))
  }

-- | A new table.  Strings longer than 'maxLen' are left alone, and the
-- table stops growing at 'maxStrings'.
newInterner :: IO Interner
newInterner = Interner <$> newArray (0, slots - 1) B.empty <*> newIORef M.empty

slots, maxLen, maxStrings :: Int
slots = 1024
maxLen = 64
maxStrings = 20000

-- | The distinct strings held.
internSize :: Interner -> IO Int
internSize t = M.size <$> readIORef (iMap t)

hash :: B.ByteString -> Int
hash v = go 0 5381
  where
    n = B.length v
    go !i !h | i >= n = (h `xor` (h `shiftR` 10)) .&. (slots - 1)
             | otherwise = go (i + 1) (h * 33 + fromIntegral (BU.unsafeIndex v i))

-- | The one copy of a string.
intern :: Interner -> B.ByteString -> B.ByteString
intern t v
  | n == 0 || n > maxLen = v
  | otherwise = unsafeDupablePerformIO $ do
      let !h = hash v
      c <- unsafeRead (iCache t) h
      if c == v then pure c else slow t h v
  where n = B.length v
{-# INLINE intern #-}

slow :: Interner -> Int -> B.ByteString -> IO B.ByteString
slow t h v = do
  m <- readIORef (iMap t)
  case M.lookup v m of
    Just c -> unsafeWrite (iCache t) h c >> pure c
    Nothing
      | M.size m >= maxStrings -> pure v
      | otherwise -> do
          let !c = B.copy v
          writeIORef (iMap t) $! M.insert c c m
          unsafeWrite (iCache t) h c
          pure c
{-# NOINLINE slow #-}

internMaybe :: Interner -> Maybe B.ByteString -> Maybe B.ByteString
internMaybe t (Just v) = Just $! intern t v
internMaybe _ Nothing = Nothing
{-# INLINE internMaybe #-}
