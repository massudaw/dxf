{-# LANGUAGE BangPatterns, OverloadedStrings #-}
-- | Text DXF to tags.  Lines end in LF or CR LF; the value of a tag is
-- the line as it is (a slice of the input), the group code is parsed.
-- The list is produced lazily: a consumer that drops tags as it goes
-- never holds the file as tags.
--
-- 'nextTag' reads one tag at a byte offset without building anything: the
-- scanner is made of it, and so are the decoders that read entities
-- straight from the bytes (see "DXF.Entity.Fast").
module DXF.Tag.Text
  ( scanText, scanFrom
  , T (..), nextTag, endCode, badCode, noValue, slice
  ) where

import Control.Exception (throw)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as U
import DXF.Tag

-- | The layout of the file (from its first tag) and its tags, up to and
-- including @0 EOF@; whatever follows is ignored, as ezdxf does.
scanText :: B.ByteString -> (Fmt, [Tag])
scanText bs = (fmt, scanFrom bs 0)
  where
    n = B.length bs
    fmt = Fmt { fmtBinary = False
              , fmtCRLF = case B.elemIndex 10 bs of
                            Just k | k > 0 -> U.unsafeIndex bs (k - 1) == 13
                            _ -> False
              , fmtPad = n > 0 && U.unsafeIndex bs 0 == 32
              , fmtR12 = False }

-- | One tag read at an offset: its code (or one of the three codes
-- below), the start and end of its value, and the offset after it.
data T = T { tCode :: {-# UNPACK #-} !Int, tStart, tEnd, tNext :: {-# UNPACK #-} !Int }

-- | Codes 'nextTag' answers with when there is no tag: only blanks left,
-- a group code line that is not a number, a group code without a value.
endCode, badCode, noValue :: Int
endCode = minBound
badCode = minBound + 1
noValue = minBound + 2

-- | The bytes of a value.
slice :: B.ByteString -> Int -> Int -> B.ByteString
slice bs s e = U.unsafeTake (e - s) (U.unsafeDrop s bs)
{-# INLINE slice #-}

nextTag :: B.ByteString -> Int -> T
nextTag bs i
  | i >= n = T endCode 0 0 n
  | otherwise =
      let e1 = lineEnd i
          c = code i e1
      in if c == badCode then (if allBlank i then T endCode 0 0 n else T badCode 0 0 i)
         else if e1 >= n then T noValue 0 0 i
         else let s2 = e1 + 1
                  e2 = lineEnd s2
                  ve = if e2 > s2 && U.unsafeIndex bs (e2 - 1) == 13 then e2 - 1 else e2
              in T c s2 ve (e2 + 1)
  where
    n = B.length bs
    lineEnd !j = case B.elemIndex 10 (U.unsafeDrop j bs) of
      Just k -> j + k
      Nothing -> n
    allBlank !j = j >= n || (blank (U.unsafeIndex bs j) && allBlank (j + 1))
    blank c = c == 32 || c == 9 || c == 10 || c == 13
    -- group code line: blanks, optional sign, digits, blanks
    code !a !e = skip a
      where
        skip !j | j < e, let c = U.unsafeIndex bs j, c == 32 || c == 9 = skip (j + 1)
                | j < e, U.unsafeIndex bs j == 45 = digits (j + 1) 0 (j + 1) True
                | otherwise = digits j 0 j False
        digits !j !acc !s !neg
          | j < e, let c = U.unsafeIndex bs j, c >= 48, c <= 57 =
              digits (j + 1) (acc * 10 + fromIntegral c - 48) s neg
          | j == s = badCode
          | otherwise = trail j (if neg then negate acc else acc)
        trail !j !v
          | j >= e = v
          | let c = U.unsafeIndex bs j, c == 32 || c == 9 || c == 13 = trail (j + 1) v
          | otherwise = badCode
{-# INLINE nextTag #-}

-- | The tags from an offset on (which must be the start of a group code
-- line), up to and including @0 EOF@.
scanFrom :: B.ByteString -> Int -> [Tag]
scanFrom bs = go
  where
    go !i = case nextTag bs i of
      T c s e nx
        | c > endCode + 2 ->
            let v = slice bs s e
            in Tag c v : (if c == 0 && v == eof then [] else go nx)
        | c == endCode -> []
        | c == badCode -> throw (DxfError (lineOf i) "invalid group code")
        | otherwise -> throw (DxfError (lineOf i + 1) "unexpected end of file: group code without value")
    lineOf i = 1 + B.count 10 (B.take i bs)
    eof = B.pack [69, 79, 70]
