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
  , entb :: String
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
  = LWPOLYLINE
  { open :: Bool
  , width :: Double
  , thickness :: Maybe Double
  , vertices :: [V2 Double]
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


