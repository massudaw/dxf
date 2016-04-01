module DXF.Types where

import Linear.V2
import Linear.V3
import Data.Map(Map)

data Entity
  = Entity
  { etype :: String
  , eref :: Object
  , evalue :: EntityTy
  }deriving (Show)

data Object
  = Object
  { handle :: Int
  , pointer :: String
  , entg :: String
  , groupcode :: Maybe String
  , layer :: String
  , linetype :: Maybe String
  , linecolor :: Maybe String
  , lineweight :: Maybe String
  , eps :: Maybe String
  , entb :: Maybe String
  }
  deriving Show

data Block
  = Block
  { block :: Object
  , name :: String
  , flag :: String
  , position :: V3 Double
  , xref :: String
  , array :: [Entity]
  , endblock :: Object
  }deriving Show

data Header
  = Header
  { rawvariables :: Map String (Map String String)
  , handseed :: Int
  }deriving Show

data EntityTy
  = POLYLINE
  {
   pposition :: V3 Double
  , pflag :: Int
  , pvertices :: [Entity]
  }
  | VERTEX
  { vtag :: String
  ,vposition :: V3 Double
  , vflag :: Int
  }
  | LWPOLYLINE
  { open :: Bool
  , width :: Double
  , thickness :: Maybe Double
  , vertices :: [(V2 Double,Maybe Double)]
  }
  | LINE
  { origin :: V3 Double
  , target :: V3 Double
  }
  | TEXT
  { origin :: V3 Double
  , theigth :: Double
  , tvalue :: String
  , trotation :: Maybe Double
  , taxis :: Maybe (V3 Double)
  }
  | CIRCLE
  { center :: V3 Double
  , radius :: Double
  }
  | INSERT
  { iblockname :: String
  , iposition ::  V3 Double
  , iscale :: Maybe (V3 Double)
  , irotation :: Maybe Double
  , irotationAxis :: Maybe (V3 Double)
  , attribs :: [Entity]
  }
  | SEQEND
  | ATTRIB
  { aposition :: V3 Double
  , aheight :: Double
  , avalue :: String
  , relativescale :: Maybe Double
  , textstyle :: Maybe String
  , eclass :: String
  , version :: Maybe String
  , attrstring :: Maybe String
  , attrflags :: Maybe Int
  , lock :: Maybe String
  }
  deriving (Show)

data DXF
  = DXF
  { header ::  Header
  , classes :: [String]
  , tables ::  [String]
  , blocks  :: [Block]
  , entities:: [Entity]
  , objects :: [String]
  , acdsdata :: [String]
  }deriving Show


