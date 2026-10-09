{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
-- | Objects of the OBJECTS section that carry data programs use:
-- DICTIONARY (and with default), DICTIONARYVAR, XRECORD, GROUP, IMAGEDEF,
-- IMAGEDEF_REACTOR, RASTERVARIABLES, WIPEOUTVARIABLES, SORTENTSTABLE,
-- ACDBPLACEHOLDER.  Others are kept as tags.
module DXF.Entity.Objects
  ( ObjBody (..), objCodecs
  , Dictionary (..), DictionaryVar (..), XRecord (..), Group (..), ImageDef (..), SortEnts (..)
  ) where

import Control.DeepSeq (NFData)
import qualified Data.ByteString as B
import GHC.Generics (Generic)
import DXF.Codec
import DXF.Json
import DXF.Number
import DXF.Tag
import DXF.Tag.Out
import DXF.Vec

data ObjBody
  = ODictionary !Dictionary
  | ODictionaryWDflt !Dictionary
  | ODictionaryVar !DictionaryVar
  | OXRecord !XRecord
  | OGroup !Group
  | OImageDef !ImageDef
  | OImageDefReactor !(Maybe Int) !(Maybe B.ByteString)   -- ^ class version (90), image handle (330)
  | ORasterVariables !(Maybe Int) !(Maybe Int) !(Maybe Int) !(Maybe Int)  -- ^ 90, frame 70, quality 71, units 72
  | OWipeoutVariables !(Maybe Int)                        -- ^ frame 70
  | OSortEnts !SortEnts
  | OPlaceholder
  deriving (Eq, Show, Generic, NFData)

data Dictionary = Dictionary
  { dcHardOwned :: !(Maybe Int)            -- ^ 280
  , dcCloning   :: !(Maybe Int)            -- ^ 281
  , dcDefault   :: !(Maybe B.ByteString)   -- ^ 340, ACDBDICTIONARYWDFLT only
  , dcEntries   :: [(B.ByteString, Int, B.ByteString)] -- ^ key (3), handle code (350 or 360), handle
  } deriving (Eq, Show, Generic, NFData)

data DictionaryVar = DictionaryVar
  { dvSchema :: !(Maybe Int)               -- ^ 280
  , dvValue  :: !B.ByteString              -- ^ 1
  } deriving (Eq, Show, Generic, NFData)

-- | XRECORD: its cloning flag and then any tags at all.
data XRecord = XRecord
  { xrCloning :: !(Maybe Int)              -- ^ 280, the first tag
  , xrTags    :: [Tag]
  } deriving (Eq, Show, Generic, NFData)

data Group = Group
  { grDescription :: !(Maybe B.ByteString) -- ^ 300
  , grUnnamed     :: !(Maybe Int)          -- ^ 70
  , grSelectable  :: !(Maybe Int)          -- ^ 71
  , grHandles     :: [B.ByteString]        -- ^ 340, the entities
  } deriving (Eq, Show, Generic, NFData)

data ImageDef = ImageDef
  { idVersion :: !(Maybe Int)              -- ^ 90
  , idFile    :: !B.ByteString             -- ^ 1
  , idSize    :: !V2                       -- ^ 10, pixels
  , idPixel   :: !(Maybe V2)               -- ^ 11, size of one pixel
  , idLoaded  :: !(Maybe Int)              -- ^ 280
  , idUnits   :: !(Maybe Int)              -- ^ 281
  } deriving (Eq, Show, Generic, NFData)

data SortEnts = SortEnts
  { seBlock :: !(Maybe B.ByteString)       -- ^ 330, the block record
  , sePairs :: [(B.ByteString, B.ByteString)] -- ^ entity handle (331), sort handle (5)
  } deriving (Eq, Show, Generic, NFData)

dictCodec :: Codec Dictionary
dictCodec = codec (Dictionary Nothing Nothing Nothing [])
  [ mark "AcDbDictionary", mark "AcDbDictionaryWithDefault"
  , mint "hard_owned" 280 (lens dcHardOwned (\r v -> r { dcHardOwned = v }))
  , mint "cloning" 281 (lens dcCloning (\r v -> r { dcCloning = v }))
  , mstr "default" 340 (lens dcDefault (\r v -> r { dcDefault = v }))
  , custom "keys" [3, 350, 360] rd (foldMap wr . dcEntries) (\r -> Just (JA [JS k | (k, _, _) <- dcEntries r]))
      (\r -> r { dcEntries = reverse (dcEntries r) }) ]
  where
    wr (k, c, h) = tS 3 k <> tS c h
    rd 3 v r = Just r { dcEntries = (v, 350, "") : dcEntries r }
    rd c v r = case dcEntries r of
      ((k, _, "") : es) -> Just r { dcEntries = (k, c, v) : es }
      _ -> Nothing

dictVarCodec :: Codec DictionaryVar
dictVarCodec = codec (DictionaryVar Nothing "")
  [ mark "DictionaryVariables"
  , mint "schema" 280 (lens dvSchema (\r v -> r { dvSchema = v }))
  , str "value" 1 (lens dvValue (\r v -> r { dvValue = v })) ]

-- | Everything after the cloning flag is data.
xrecordCodec :: Codec XRecord
xrecordCodec = codec (XRecord Nothing [])
  [ mark "AcDbXrecord"
  , custom "cloning" ([1 .. 99] ++ [102 .. 1071]) rd (\r -> maybe mempty (tI 280) (xrCloning r) <> tTags (xrTags r))
      (fmap JI . xrCloning) (\r -> r { xrTags = reverse (xrTags r) }) ]
  where
    rd 280 v r | Nothing <- xrCloning r, null (xrTags r) = (\n -> r { xrCloning = Just n }) <$> readInt v
    rd c v r = Just r { xrTags = Tag c v : xrTags r }

groupCodec :: Codec Group
groupCodec = codec (Group Nothing Nothing Nothing [])
  [ mark "AcDbGroup"
  , mstr "description" 300 (lens grDescription (\r v -> r { grDescription = v }))
  , mint "unnamed" 70 (lens grUnnamed (\r v -> r { grUnnamed = v }))
  , mint "selectable" 71 (lens grSelectable (\r v -> r { grSelectable = v }))
  , strs "" 340 (lens grHandles (\r v -> r { grHandles = v })) ]

imageDefCodec :: Codec ImageDef
imageDefCodec = codec (ImageDef Nothing "" v2zero Nothing Nothing Nothing)
  [ mark "AcDbRasterImageDef"
  , mint "class_version" 90 (lens idVersion (\r v -> r { idVersion = v }))
  , str "filename" 1 (lens idFile (\r v -> r { idFile = v }))
  , pt2 "image_size" 10 (lens idSize (\r v -> r { idSize = v }))
  , mpt2 "pixel_size" 11 (lens idPixel (\r v -> r { idPixel = v }))
  , mint "loaded" 280 (lens idLoaded (\r v -> r { idLoaded = v }))
  , mint "resolution_units" 281 (lens idUnits (\r v -> r { idUnits = v })) ]

reactorCodec :: Codec (Maybe Int, Maybe B.ByteString)
reactorCodec = codec (Nothing, Nothing)
  [ mark "AcDbRasterImageDefReactor"
  , mint "class_version" 90 (lens fst (\r v -> (v, snd r)))
  , mstr "image_handle" 330 (lens snd (\r v -> (fst r, v))) ]

rasterCodec :: Codec (Maybe Int, Maybe Int, Maybe Int, Maybe Int)
rasterCodec = codec (Nothing, Nothing, Nothing, Nothing)
  [ mark "AcDbRasterVariables"
  , mint "class_version" 90 (lens (\(a, _, _, _) -> a) (\(_, b, c, d) v -> (v, b, c, d)))
  , mint "frame" 70 (lens (\(_, b, _, _) -> b) (\(a, _, c, d) v -> (a, v, c, d)))
  , mint "quality" 71 (lens (\(_, _, c, _) -> c) (\(a, b, _, d) v -> (a, b, v, d)))
  , mint "units" 72 (lens (\(_, _, _, d) -> d) (\(a, b, c, _) v -> (a, b, c, v))) ]

wipeoutVarsCodec :: Codec (Maybe Int)
wipeoutVarsCodec = codec Nothing [mark "AcDbWipeoutVariables", mint "frame" 70 (lens id (\_ v -> v))]

sortCodec :: Codec SortEnts
sortCodec = codec (SortEnts Nothing [])
  [ mark "AcDbSortentsTable"
  , mstr "block_record_handle" 330 (lens seBlock (\r v -> r { seBlock = v }))
  , custom "" [5, 331] rd (foldMap (\(e, s) -> tS 331 e <> tS 5 s) . sePairs) (const Nothing)
      (\r -> r { sePairs = reverse (sePairs r) }) ]
  where
    -- the entity handle (331) comes first, then the sort handle (5)
    rd 331 v r = Just r { sePairs = (v, "") : sePairs r }
    rd _ v r = case sePairs r of
      ((e, "") : ps) -> Just r { sePairs = (e, v) : ps }
      _ -> Nothing

objCodecs :: [BodyCodec ObjBody]
objCodecs =
  [ entityCodec "DICTIONARY" False dictCodec ODictionary (\g -> case g of ODictionary r -> Just r; _ -> Nothing)
  , entityCodec "ACDBDICTIONARYWDFLT" False dictCodec ODictionaryWDflt (\g -> case g of ODictionaryWDflt r -> Just r; _ -> Nothing)
  , entityCodec "DICTIONARYVAR" False dictVarCodec ODictionaryVar (\g -> case g of ODictionaryVar r -> Just r; _ -> Nothing)
  , entityCodec "XRECORD" False xrecordCodec OXRecord (\g -> case g of OXRecord r -> Just r; _ -> Nothing)
  , entityCodec "GROUP" False groupCodec OGroup (\g -> case g of OGroup r -> Just r; _ -> Nothing)
  , entityCodec "IMAGEDEF" False imageDefCodec OImageDef (\g -> case g of OImageDef r -> Just r; _ -> Nothing)
  , entityCodec "IMAGEDEF_REACTOR" False reactorCodec (uncurry OImageDefReactor) (\g -> case g of OImageDefReactor a b -> Just (a, b); _ -> Nothing)
  , entityCodec "RASTERVARIABLES" False rasterCodec (\(a, b, c, d) -> ORasterVariables a b c d) (\g -> case g of ORasterVariables a b c d -> Just (a, b, c, d); _ -> Nothing)
  , entityCodec "WIPEOUTVARIABLES" False wipeoutVarsCodec OWipeoutVariables (\g -> case g of OWipeoutVariables r -> Just r; _ -> Nothing)
  , entityCodec "SORTENTSTABLE" False sortCodec OSortEnts (\g -> case g of OSortEnts r -> Just r; _ -> Nothing)
  , entityCodec "ACDBPLACEHOLDER" False (codec () []) (const OPlaceholder) (\g -> case g of OPlaceholder -> Just (); _ -> Nothing) ]
