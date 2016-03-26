{-# LANGUAGE NoMonomorphismRestriction,OverloadedStrings #-}
module DXF.Parser where

import Debug.Trace
import Data.Monoid
import Control.Monad
import Linear.V2
import Linear.V3
import GHC.Word
import Data.Attoparsec.Char8 as A

import qualified Data.ByteString.Char8 as B

readChar c = A.takeWhile (/=c) >> char8 c >> endOfLine

readCode c = manyTill A.anyChar (string c)  >> endOfLine

main = do
  B.readFile "PATH.DXF" >>= print . fmap entities . parseOnly dxf


section :: B.ByteString  -> Parser [String]
section n = do
  readChar '0'  >> string "SECTION" >> endOfLine >> readChar '2' >> string n >> endOfLine
  manyTill (manyTill A.anyChar endOfLine)  (readChar '0' >> string "ENDSEC")


data Entity
  = Entity
  { etype :: String
  , elayer :: String
  , evalue :: EntityTy
  }deriving (Show)

data EntityTy
  = LWPOLYLINE
  { open :: Bool
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
  { header ::  [String]
  , classes :: [String]
  , tables ::  [String]
  , blocks  :: [String]
  , entities:: [Entity]
  , objects :: [String]
  }deriving Show

dxf = do
  h <- section "HEADER" >>= parseHeader
  c <- section "CLASSES" >>= parseHeader
  t <- section "TABLES" >>= parseHeader
  b <- section "BLOCKS" >>= parseHeader
  e <- section "ENTITIES" >>= parseEntity
  o <- section "OBJECTS" >>= parseHeader
  return  $ DXF h c t b e o

line = manyTill A.anyChar endOfLine
icode i = readCode i >> line

parseEnt = do
  readCode "0"
  t <- line
  icode "5"
  icode "330"
  icode "100"
  readCode "8"
  l <- line
  icode "100"
  Entity t l <$> case traceShow l $ traceShowId t of
    "LWPOLYLINE" -> do
      readCode "90"
      n <- line
      readCode "70"
      b <- line
      icode "43"
      m <- replicateM (read n) (do
        readCode "10"
        x <- line
        readCode "20"
        y <- line
        return (read <$> V2 x  y))
      return (LWPOLYLINE (if b /= "1" then True else False) m)
    "LINE" -> do
      readCode "10"
      ax <- line
      readCode "20"
      ay <- line
      readCode "30"
      az <- line
      readCode "11"
      bx <- line
      readCode "21"
      by <- line
      readCode "31"
      bz <- line
      return (LINE (read <$> V3 ax ay az) (read <$> V3 bx by bz))
    "CIRCLE" -> do
      readCode "10"
      ax <- line
      readCode "20"
      ay <- line
      readCode "30"
      az <- line
      readCode "40"
      rz <- line
      return (CIRCLE  (read <$> V3 ax ay az) (read rz))
    i -> error i



parseEntity :: [String] -> Parser [Entity]
parseEntity t= do
  case parseOnly  (many1  parseEnt) (B.pack $ unlines t ) of
    Right i -> return i
    Left  i -> error (i <> show t)

parseHeader t= do
  return t


