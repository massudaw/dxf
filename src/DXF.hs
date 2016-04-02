module DXF (
  addEntity,
  module DXF.Types ,
  module DXF.Writer ,
  module DXF.Parser ) where

import DXF.Writer
import DXF.Parser
import DXF.Types


idDXF = do
  Right v <- readDXF "PATH.DXF"
  print (entities v)
  writeDXF "PATHW.DXF" v

incSeed (Header m s) = (s,Header m (s+1))

addEntity en dxf = dxf { header = nh , entities = e:(entities  dxf)}
  where e = en s
        (s,nh ) = incSeed (header dxf)



