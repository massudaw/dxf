{-# LANGUAGE OverloadedStrings #-}
-- | Extended data: what an application hangs on any entity, table entry or
-- object after its own tags -- a @1001@ application name, then typed values
-- (@1000@ strings, @1040@ reals, @1070@ and @1071@ integers, @1010@ points,
-- @1002@ braces...).  An application must be registered in the APPID table
-- (the builder in "DXF.Build" does that when data is attached).
module DXF.XData
  ( XValue (..), XData (..)
  , xdataTags, xdataFromTags
  , entityXData, setXData, appXData, xdataFor
  , xdataProblem, intBytes, realBytes
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Builder.Extra as BE
import qualified Data.ByteString.Lazy as BL
import DXF.Entity
import DXF.Number
import DXF.Tag
import DXF.Tag.Out (doubleText)
import DXF.Vec

-- | One value of extended data, by its group code.
data XValue
  = XStr !B.ByteString        -- ^ 1000, at most 255 bytes
  | XLayer !B.ByteString      -- ^ 1003, a layer name
  | XHandle !B.ByteString     -- ^ 1005, a database handle (hex text)
  | XReal !Double             -- ^ 1040
  | XInt !Int                 -- ^ 1070, 16 bit
  | XLong !Int                -- ^ 1071, 32 bit
  | XPoint !Int !V3           -- ^ 1010 to 1013: a position (1010), world position, displacement, direction
  | XOpen | XClose            -- ^ 1002 @{@ and @}@
  | XOther !Int !B.ByteString -- ^ any other code, as it was (1004 binary, 1041 distance, 1042 scale)
  deriving (Eq, Show)

-- | The values one application attached.
data XData = XData
  { xdApp    :: !B.ByteString
  , xdValues :: [XValue]
  } deriving (Eq, Show)

-- | The tags: @1001@, then the values.
xdataTags :: XData -> [Tag]
xdataTags (XData app vs) = Tag 1001 app : concatMap valueTags vs

valueTags :: XValue -> [Tag]
valueTags v = case v of
  XStr s -> [Tag 1000 s]
  XLayer s -> [Tag 1003 s]
  XHandle s -> [Tag 1005 s]
  XReal d -> [Tag 1040 (realBytes d)]
  XInt i -> [Tag 1070 (intBytes i)]
  XLong i -> [Tag 1071 (intBytes i)]
  XPoint c (V3 x y z) -> [Tag c (realBytes x), Tag (c + 10) (realBytes y), Tag (c + 20) (realBytes z)]
  XOpen -> [Tag 1002 "{"]
  XClose -> [Tag 1002 "}"]
  XOther c s -> [Tag c s]

-- | Extended data from the tags of an entity (as 'cXData' holds them): every
-- @1001@ starts one application's values.
xdataFromTags :: [Tag] -> [XData]
xdataFromTags (Tag 1001 app : rest) =
  let (vs, rest') = break (\(Tag c _) -> c == 1001) rest in XData app (values vs) : xdataFromTags rest'
xdataFromTags (_ : rest) = xdataFromTags rest
xdataFromTags [] = []

values :: [Tag] -> [XValue]
values ts = case ts of
  [] -> []
  Tag 1000 s : r -> XStr s : values r
  Tag 1003 s : r -> XLayer s : values r
  Tag 1005 s : r -> XHandle s : values r
  Tag 1040 s : r | Just d <- readDouble s -> XReal d : values r
  Tag 1070 s : r | Just i <- readInt s -> XInt i : values r
  Tag 1071 s : r | Just i <- readInt s -> XLong i : values r
  Tag 1002 s : r -> (if s == "}" then XClose else XOpen) : values r
  Tag c x : Tag c' y : Tag c'' z : r
    | c >= 1010, c <= 1013, c' == c + 10, c'' == c + 20
    , Just a <- readDouble x, Just b <- readDouble y, Just d <- readDouble z -> XPoint c (V3 a b d) : values r
  Tag c s : r -> XOther c s : values r

-- | The extended data of an entity, one 'XData' per application.
entityXData :: Entity -> [XData]
entityXData = xdataFromTags . cXData . eCommon

-- | Replace the extended data of an entity.
setXData :: [XData] -> Entity -> Entity
setXData xs e = e { eCommon = (eCommon e) { cXData = concatMap xdataTags xs } }

-- | What one application attached to an entity.
appXData :: B.ByteString -> Entity -> [[XValue]]
appXData app e = [vs | XData a vs <- entityXData e, a == app]

-- | The values of the first piece of data an application attached.
xdataFor :: B.ByteString -> Entity -> Maybe [XValue]
xdataFor app e = case appXData app e of (vs : _) -> Just vs; [] -> Nothing

-- | Why this data cannot be written, if it cannot: AutoCAD refuses strings of
-- more than 255 bytes and any line break inside a string.
xdataProblem :: XData -> Maybe String
xdataProblem (XData app vs)
  | B.null app || B.length app > 31 = Just ("application name " ++ show app ++ " must be 1 to 31 bytes")
  | any bad (app : [s | v <- vs, Just s <- [str v]]) = Just ("string with a line break or over 255 bytes in " ++ show app)
  | otherwise = Nothing
  where
    bad s = B.length s > 255 || B.elem 10 s || B.elem 13 s
    str v = case v of XStr s -> Just s; XLayer s -> Just s; XHandle s -> Just s; _ -> Nothing

realBytes :: Double -> B.ByteString
realBytes = BL.toStrict . BE.toLazyByteStringWith (BE.untrimmedStrategy 32 64) BL.empty . doubleText

-- | An integer's decimal text.
intBytes :: Int -> B.ByteString
intBytes n
  | n < 0 = B.cons 45 (intBytes (negate n))
  | n < 10 = B.singleton (48 + fromIntegral n)
  | otherwise = B.pack (go n [])
  where go 0 acc = acc
        go k acc = let (q, r) = k `quotRem` 10 in go q (48 + fromIntegral r : acc)
