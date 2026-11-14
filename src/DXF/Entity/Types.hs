{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
-- | An entity: anything of the form @0 TYPE ...@ in ENTITIES, BLOCKS,
-- OBJECTS or a table -- graphical entity, table entry, object.
module DXF.Entity.Types
  ( Entity (..), Body (..)
  ) where

import Control.DeepSeq (NFData)
import qualified Data.ByteString as B
import GHC.Generics (Generic)
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
import DXF.Tag (Tag)

data Entity = Entity
  { eType   :: !B.ByteString  -- ^ @LINE@, @LAYER@, @DICTIONARY@...
  , eCommon :: !Common
  , eBody   :: !Body
  , eKids   :: [Entity]       -- ^ POLYLINE: its VERTEXes and the SEQEND; INSERT: its ATTRIBs and the SEQEND
  } deriving (Eq, Show, Generic, NFData)

-- | What is typed about an entity, by group of types.  A type the library
-- has no codec for is 'BRaw': all its tags after the @0 TYPE@ tag, as read.
data Body
  = BGeo !GeoBody
  | BTxt !TxtBody
  | BPoly !PolyBody
  | BHatch !HatchBody
  | BDim !DimBody
  | BMisc !MiscBody
  | BObj !ObjBody
  | BSet !SetBody
  | BTab !TabBody
  | BBlk !BlkBody
  | BRaw [Tag]
  deriving (Eq, Show, Generic, NFData)
