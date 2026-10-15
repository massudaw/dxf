{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
-- | The CLASSES section: one @0 CLASS@ record per class.
module DXF.Classes
  ( Class (..), classFromTags, classOut, classDump
  ) where

import Control.DeepSeq (NFData)
import qualified Data.ByteString as B
import GHC.Generics (Generic)
import DXF.Codec
import DXF.Json
import DXF.Tag
import DXF.Tag.Out

data Class = Class
  { clsDxfName    :: !B.ByteString  -- ^ 1, record name in the DXF file (@ACDBDICTIONARYWDFLT@)
  , clsCppName    :: !B.ByteString  -- ^ 2
  , clsApp        :: !B.ByteString  -- ^ 3, application name
  , clsProxyFlags :: !Int           -- ^ 90
  , clsInstances  :: !(Maybe Int)   -- ^ 91
  , clsWasProxy   :: !(Maybe Int)   -- ^ 280
  , clsIsEntity   :: !(Maybe Int)   -- ^ 281
  , clsExtra      :: [Tag]          -- ^ whatever else the record held
  } deriving (Eq, Show, Generic, NFData)

classCodec :: Codec Class
classCodec = codec (Class "" "" "" 0 Nothing Nothing Nothing [])
  [ str "name" 1 (lens clsDxfName (\r v -> r { clsDxfName = v }))
  , str "cpp_class_name" 2 (lens clsCppName (\r v -> r { clsCppName = v }))
  , str "app_name" 3 (lens clsApp (\r v -> r { clsApp = v }))
  , int "flags" 90 (lens clsProxyFlags (\r v -> r { clsProxyFlags = v }))
  , mint "instance_count" 91 (lens clsInstances (\r v -> r { clsInstances = v }))
  , mint "was_a_proxy" 280 (lens clsWasProxy (\r v -> r { clsWasProxy = v }))
  , mint "is_an_entity" 281 (lens clsIsEntity (\r v -> r { clsIsEntity = v })) ]

-- | A class from its tags (after @0 CLASS@).
classFromTags :: [Tag] -> Class
classFromTags ts = let (c, left) = decodeWith classCodec ts in c { clsExtra = left }

classOut :: Class -> Out
classOut c = tS 0 "CLASS" <> encodeWith classCodec c <> tTags (clsExtra c)

classDump :: Class -> J
classDump c = JO (dumpWith classCodec c)
