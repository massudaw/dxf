{-# LANGUAGE OverloadedStrings #-}
-- | Entities from tags and back.  The registry maps a DXF type name to
-- the codec of the group that has it; a type nobody has is kept as tags.
module DXF.Entity
  ( module DXF.Entity.Types
  , module DXF.Entity.Common
  , decodeEntity, encodeEntity, dumpEntity, linkEntities
  , entityTypes
  ) where

import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import DXF.Codec
import DXF.Entity.Block
import DXF.Entity.Common
import DXF.Entity.Dimension
import DXF.Entity.Geometry
import DXF.Entity.Hatch
import DXF.Entity.Misc
import DXF.Entity.Objects
import DXF.Entity.Poly
import DXF.Entity.Settings
import DXF.Entity.Tables
import DXF.Entity.Text
import DXF.Entity.Types
import DXF.Json
import DXF.Tag
import DXF.Tag.Out

-- | A group's codec with the group's body type wrapped into 'Body'.
data EC = EC
  { ecGraphic :: !Bool
  , ecDecode  :: [Tag] -> (Body, [Tag])
  , ecEncode  :: Body -> Out
  , ecDump    :: Body -> [(B.ByteString, J)]
  }

lift :: (g -> Body) -> (Body -> Maybe g) -> BodyCodec g -> (B.ByteString, EC)
lift wrap unwrap bc = (bcType bc, EC (bcGraphic bc)
  (\ts -> let (g, left) = bcDecode bc ts in (wrap g, left))
  (maybe mempty (bcEncode bc) . unwrap)
  (maybe [] (bcDump bc) . unwrap))

registry :: M.Map B.ByteString EC
registry = M.fromList $ concat
  [ map (lift BGeo (\b -> case b of BGeo g -> Just g; _ -> Nothing)) geoCodecs
  , map (lift BTxt (\b -> case b of BTxt g -> Just g; _ -> Nothing)) txtCodecs
  , map (lift BPoly (\b -> case b of BPoly g -> Just g; _ -> Nothing)) polyCodecs
  , map (lift BHatch (\b -> case b of BHatch g -> Just g; _ -> Nothing)) hatchCodecs
  , map (lift BDim (\b -> case b of BDim g -> Just g; _ -> Nothing)) dimCodecs
  , map (lift BMisc (\b -> case b of BMisc g -> Just g; _ -> Nothing)) miscCodecs
  , map (lift BObj (\b -> case b of BObj g -> Just g; _ -> Nothing)) objCodecs
  , map (lift BSet (\b -> case b of BSet g -> Just g; _ -> Nothing)) setCodecs
  , map (lift BTab (\b -> case b of BTab g -> Just g; _ -> Nothing)) tabCodecs
  , map (lift BBlk (\b -> case b of BBlk g -> Just g; _ -> Nothing)) blkCodecs
  ]

-- | The type names the library has a codec for.
entityTypes :: [B.ByteString]
entityTypes = M.keys registry

-- | An entity from its tags, the first of which is @0 TYPE@.  Children
-- (vertices, attributes) are linked by 'linkEntities'.
decodeEntity :: [Tag] -> Entity
decodeEntity [] = Entity "" blankCommon (BRaw []) []
decodeEntity (Tag _ ty : rest) = case M.lookup ty registry of
  Nothing -> Entity ty blankCommon (BRaw rest) []
  Just ec ->
    let (c, own) = splitTags (ecGraphic ec) rest
        (b, left) = ecDecode ec own
    in Entity ty c { cExtra = left } b []

-- | Write an entity and its children.
encodeEntity :: Entity -> Out
encodeEntity (Entity ty c b kids) = case b of
  BRaw ts -> tS 0 ty <> tTags ts <> foldMap encodeEntity kids
  _ -> case M.lookup ty registry of
    Just ec -> commonHead (ecGraphic ec) ty c <> ecEncode ec b <> commonTail c <> foldMap encodeEntity kids
    Nothing -> mempty

-- | What the library found, for the oracle: type, attributes named as ezdxf
-- names them, children.
dumpEntity :: Entity -> J
dumpEntity (Entity ty c b kids) = JO
  [ ("type", JS ty)
  , ("attribs", JO (case b of
       BRaw ts -> [("raw", JI (length ts))]
       _ -> commonDump c ++ maybe [] (\ec -> ecDump ec b) (M.lookup ty registry)))
  , ("kids", JA (map dumpEntity kids)) ]

-- | Attach the VERTEXes and SEQEND that follow a POLYLINE, the ATTRIBs and
-- SEQEND that follow an INSERT, as that entity's children.
linkEntities :: [Entity] -> [Entity]
linkEntities [] = []
linkEntities (e : es)
  | eType e == "POLYLINE" = attach "VERTEX"
  | eType e == "INSERT" = attach "ATTRIB"
  | otherwise = e : linkEntities es
  where
    attach child =
      let (cs, rest) = span ((== child) . eType) es
          (end, rest') = case rest of
            (s : r) | eType s == "SEQEND" -> ([s], r)
            _ -> ([], rest)
      in e { eKids = cs ++ end } : linkEntities rest'
