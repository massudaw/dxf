{-# LANGUAGE BangPatterns #-}
-- | Text DXF to tags.  Lines end in LF or CR LF; the value of a tag is
-- the line as it is (a slice of the input), the group code is parsed.
-- The list is produced lazily: a consumer that drops tags as it goes
-- never holds the file as tags.
module DXF.Tag.Text
  ( scanText
  ) where

import Control.Exception (throw)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as U
import DXF.Tag

-- | The layout of the file (from its first tag) and its tags, up to and
-- including @0 EOF@; whatever follows is ignored, as ezdxf does.
scanText :: B.ByteString -> (Fmt, [Tag])
scanText bs = (fmt, go 0 1)
  where
    n = B.length bs
    fmt = Fmt { fmtBinary = False
              , fmtCRLF = case B.elemIndex 10 bs of
                            Just k | k > 0 -> U.unsafeIndex bs (k - 1) == 13
                            _ -> False
              , fmtPad = n > 0 && U.unsafeIndex bs 0 == 32
              , fmtR12 = False }
    lineEnd !i = case B.elemIndex 10 (U.unsafeDrop i bs) of
      Just k -> i + k
      Nothing -> n
    allBlank !i = i >= n || (blank (U.unsafeIndex bs i) && allBlank (i + 1))
    blank c = c == 32 || c == 9 || c == 10 || c == 13
    go !i !ln
      | i >= n = []
      | otherwise =
          let e1 = lineEnd i
          in case code i e1 of
               Nothing | allBlank i -> []
                       | otherwise -> throw (DxfError ln "invalid group code")
               Just c
                 | e1 >= n -> throw (DxfError (ln + 1) "unexpected end of file: group code without value")
                 | otherwise ->
                     let s2 = e1 + 1
                         e2 = lineEnd s2
                         ve = if e2 > s2 && U.unsafeIndex bs (e2 - 1) == 13 then e2 - 1 else e2
                         v = U.unsafeTake (ve - s2) (U.unsafeDrop s2 bs)
                     in Tag c v : (if c == 0 && v == eof then [] else go (e2 + 1) (ln + 2))
    eof = B.pack [69, 79, 70]
    -- group code line: blanks, optional sign, digits, blanks
    code !i !e = skip i
      where
        skip !j | j < e, let c = U.unsafeIndex bs j, c == 32 || c == 9 = skip (j + 1)
                | j < e, U.unsafeIndex bs j == 45 = digits (j + 1) 0 False (j + 1) True
                | otherwise = digits j 0 False j False
        digits !j !acc !_ !s !neg
          | j < e, let c = U.unsafeIndex bs j, c >= 48, c <= 57 =
              digits (j + 1) (acc * 10 + fromIntegral c - 48) True s neg
          | j == s = Nothing
          | otherwise = trail j (if neg then negate acc else acc)
        trail !j !v
          | j >= e = Just v
          | let c = U.unsafeIndex bs j, c == 32 || c == 9 || c == 13 = trail (j + 1) v
          | otherwise = Nothing
