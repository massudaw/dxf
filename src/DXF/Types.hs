module DXF.Types where

import Linear.V2
import Linear.V3
import Data.Map(Map)
data Entity
  = Entity
  { etype :: String
  , handler :: Int
  , pointer :: String
  , elayer :: String
  , entityG :: String
  , entityE :: String
  , evalue :: EntityTy
  }deriving (Show)

data Header
  = Header
  { rawvariables :: Map String (Map String String)
  , handseed :: Int
  }deriving Show

data EntityTy
  = LWPOLYLINE
  { open :: Bool
  , width :: Double
  , vertices :: [V2 Double]
  }
  | LINE
  { origin :: V3 Double
  , target :: V3 Double
  }
  | CIRCLE
  { center :: V3 Double
  , radius :: Double
  }
  deriving (Show)

data DXF
  = DXF
  { header ::  Header
  , classes :: [String]
  , tables ::  [String]
  , blocks  :: [String]
  , entities:: [Entity]
  , objects :: [String]
  , acdsdata :: [String]
  }deriving Show


