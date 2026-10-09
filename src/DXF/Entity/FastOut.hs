{-# LANGUAGE BangPatterns, OverloadedStrings #-}
-- | A LINE written as one primitive, the whole entity in a single call
-- into the output buffer: the layer, handle and the six numbers of the
-- usual LINE.  Anything unusual (application data, extended data,
-- unplaced tags, long strings, numbers outside plain decimal, binary
-- output) is left to the general writer, which says the same.
module DXF.Entity.FastOut
  ( fastLineOut
  ) where

import qualified Data.ByteString.Builder as BB
import Data.ByteString.Builder.Prim (primBounded)
import Data.ByteString.Builder.Prim.Internal (boundedPrim)
import Data.Word (Word8)
import Foreign.Ptr (Ptr)
import DXF.Entity.Common
import DXF.Entity.Geometry
import DXF.Entity.Types
import DXF.Tag (Fmt (..))
import DXF.Tag.Prim
import DXF.Vec

-- | The LINE as one builder, or 'Nothing' when the general writer must do it.
fastLineOut :: Fmt -> Entity -> Maybe BB.Builder
fastLineOut f (Entity "LINE" c (BGeo (GLine (Line a b Nothing Nothing))) [])
  | not (fmtBinary f), usual c, ok3 a, ok3 b =
      Just (primBounded (boundedPrim 1700 (body (fmtPad f) (fmtCRLF f) (fmtR12 f) c)) (a, b))
  where ok3 (V3 x y z) = plainDouble x && plainDouble y && plainDouble z
fastLineOut _ _ = Nothing

-- | The head is a 'plainHead' of a graphical entity, with no extended data
-- or unplaced tags after it.
usual :: Common -> Bool
usual c = plainHead "LINE" c && null (cXData c) && null (cExtra c)

body :: Bool -> Bool -> Bool -> Common -> (V3, V3) -> Ptr Word8 -> IO (Ptr Word8)
body pad crlf r12 c (V3 ax ay az, V3 bx by bz) p0 = do
  p1 <- headWriter pad crlf r12 True "LINE" c p0
  p2 <- if r12 then pure p1 else putBytesTag pad crlf 100 "AcDbLine" p1
  p3 <- putDoubleTag pad crlf 10 ax p2
  p4 <- putDoubleTag pad crlf 20 ay p3
  p5 <- putDoubleTag pad crlf 30 az p4
  p6 <- putDoubleTag pad crlf 11 bx p5
  p7 <- putDoubleTag pad crlf 21 by p6
  putDoubleTag pad crlf 31 bz p7
