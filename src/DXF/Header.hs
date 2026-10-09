{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
-- | The HEADER section: variables, each a name (@$ACADVER@) and the tag
-- or tags that hold its value (a point is three tags).  Every variable is
-- kept, in order, whether or not the library knows it; the typed readers
-- take the type from the group code.
module DXF.Header
  ( Header (..), HeaderVar (..)
  , headerFromTags, headerOut
  , hvLookup, hvInt, hvDouble, hvStr, hvPoint3, hvPoint2
  , headerVersion, headerCodepage, headerIsR12, hvSet
  ) where

import Control.DeepSeq (NFData)
import qualified Data.ByteString as B
import GHC.Generics (Generic)
import DXF.Number
import DXF.Tag
import DXF.Tag.Out
import DXF.Vec

data HeaderVar = HeaderVar
  { hvName :: !B.ByteString   -- ^ with the dollar sign
  , hvTags :: [Tag]           -- ^ the value tags
  } deriving (Eq, Show, Generic, NFData)

data Header = Header
  { hdrLead :: [Tag]          -- ^ tags before the first variable (comments)
  , hdrVars :: [HeaderVar]
  } deriving (Eq, Show, Generic, NFData)

-- | The tags of the section (between @2 HEADER@ and @0 ENDSEC@).
headerFromTags :: [Tag] -> Header
headerFromTags ts = let (lead, rest) = break isVar ts in Header lead (vars rest)
  where
    isVar (Tag 9 _) = True
    isVar _ = False
    vars (Tag 9 n : r) = let (vs, r') = break isVar r in HeaderVar n vs : vars r'
    vars _ = []

headerOut :: Header -> Out
headerOut (Header lead vs) = tTags lead <> foldMap (\(HeaderVar n ts) -> tS 9 n <> tTags ts) vs

hvLookup :: B.ByteString -> Header -> Maybe [Tag]
hvLookup n h = case [ts | HeaderVar n' ts <- hdrVars h, n' == n] of
  (ts : _) -> Just ts
  [] -> Nothing

hvInt :: B.ByteString -> Header -> Maybe Int
hvInt n h = hvLookup n h >>= \ts -> case ts of (Tag _ v : _) -> readInt v; _ -> Nothing

hvDouble :: B.ByteString -> Header -> Maybe Double
hvDouble n h = hvLookup n h >>= \ts -> case ts of (Tag _ v : _) -> readDouble v; _ -> Nothing

hvStr :: B.ByteString -> Header -> Maybe B.ByteString
hvStr n h = hvLookup n h >>= \ts -> case ts of (Tag _ v : _) -> Just v; _ -> Nothing

hvPoint3 :: B.ByteString -> Header -> Maybe V3
hvPoint3 n h = hvLookup n h >>= \ts -> case ts of
  [Tag _ x, Tag _ y, Tag _ z] -> V3 <$> readDouble x <*> readDouble y <*> readDouble z
  _ -> Nothing

hvPoint2 :: B.ByteString -> Header -> Maybe V2
hvPoint2 n h = hvLookup n h >>= \ts -> case ts of
  (Tag _ x : Tag _ y : _) -> V2 <$> readDouble x <*> readDouble y
  _ -> Nothing

-- | Set a variable (replacing it, or adding it at the end).
hvSet :: B.ByteString -> [Tag] -> Header -> Header
hvSet n ts h
  | any ((== n) . hvName) (hdrVars h) = h { hdrVars = [if hvName v == n then v { hvTags = ts } else v | v <- hdrVars h] }
  | otherwise = h { hdrVars = hdrVars h ++ [HeaderVar n ts] }

-- | @$ACADVER@, @AC1009@ when absent (as ezdxf).
headerVersion :: Header -> B.ByteString
headerVersion = maybe "AC1009" id . hvStr "$ACADVER"

headerCodepage :: Header -> Maybe B.ByteString
headerCodepage = hvStr "$DWGCODEPAGE"

-- | R12 and older: no subclass markers.
headerIsR12 :: Header -> Bool
headerIsR12 h = headerVersion h <= "AC1009"
