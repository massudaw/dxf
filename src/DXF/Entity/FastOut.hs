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
      Just (primBounded (boundedPrim 1100 (body (fmtPad f) (fmtCRLF f) (fmtR12 f) c)) (a, b))
  where ok3 (V3 x y z) = plainDouble x && plainDouble y && plainDouble z
fastLineOut _ _ = Nothing

-- | Only handle, owner, layer, linetype, colour and lineweight set.
usual :: Common -> Bool
usual c = null (cAppData c) && null (cXData c) && null (cExtra c) && cMore c == blankMore
  && cPaperspace c == Nothing && cLtScale c == Nothing
  && sb (cHandle c) && sb (cOwner c) && sb (cLayer c) && sb (cLinetype c)
  where sb = maybe True shortBytes

body :: Bool -> Bool -> Bool -> Common -> (V3, V3) -> Ptr Word8 -> IO (Ptr Word8)
body pad crlf r12 c (V3 ax ay az, V3 bx by bz) p0 = do
  p1 <- bytes 0 "LINE" p0
  p2 <- opt 5 (cHandle c) p1
  p3 <- opt 330 (cOwner c) p2
  p4 <- if r12 then pure p3 else bytes 100 "AcDbEntity" p3
  p5 <- opt 8 (cLayer c) p4
  p6 <- opt 6 (cLinetype c) p5
  p7 <- optI 62 (cColor c) p6
  p8 <- optI 370 (cLineweight c) p7
  p9 <- if r12 then pure p8 else bytes 100 "AcDbLine" p8
  p10 <- putDoubleTag pad crlf 10 ax p9
  p11 <- putDoubleTag pad crlf 20 ay p10
  p12 <- putDoubleTag pad crlf 30 az p11
  p13 <- putDoubleTag pad crlf 11 bx p12
  p14 <- putDoubleTag pad crlf 21 by p13
  putDoubleTag pad crlf 31 bz p14
  where
    bytes = putBytesTag pad crlf
    opt k m p = maybe (pure p) (\s -> putBytesTag pad crlf k s p) m
    optI k m p = maybe (pure p) (\i -> putIntTag pad crlf k i p) m
