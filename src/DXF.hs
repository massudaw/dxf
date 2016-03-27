module DXF (module DXF.Types , module DXF.Writer , module DXF.Parser ) where

import DXF.Writer
import DXF.Parser
import DXF.Types


idDXF = do
  Right v <- readDXF "PATH.DXF"
  print (objects v)
  writeDXF "PATHW.DXF" v



