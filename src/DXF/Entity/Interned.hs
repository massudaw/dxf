{-# LANGUAGE OverloadedStrings #-}
-- | An entity with its repeated strings interned ("DXF.Intern"): type
-- name, layer, linetype and owner, application names of extended data, and
-- the names an entity refers to by name (text style, block, dimension style,
-- the linetype of a layer).  Handles and text are unique and
-- are left as they are.  The fast decoders intern these as they read; this
-- is the same for what the general decoder made.
module DXF.Entity.Interned
  ( internEntity, internClass
  ) where

import DXF.Classes
import DXF.Entity.Common
import DXF.Entity.Dimension
import DXF.Entity.Poly
import DXF.Entity.Tables
import DXF.Entity.Text
import DXF.Entity.Types
import DXF.Intern
import DXF.Tag

internEntity :: Interner -> Entity -> Entity
internEntity t (Entity ty c b kids) =
  Entity (intern t ty) (internCommon t c) (internBody t b) kids

internCommon :: Interner -> Common -> Common
internCommon t c = c
  { cOwner = m (cOwner c), cLayer = m (cLayer c), cLinetype = m (cLinetype c)
  , cXData = map app (cXData c) }
  where
    m = internMaybe t
    app (Tag 1001 v) = Tag 1001 (intern t v)
    app x = x

internBody :: Interner -> Body -> Body
internBody t b = case b of
  BTxt (TText x) -> BTxt (TText x { txData = td (txData x) })
  BTxt (TAttrib a) -> BTxt (TAttrib (attr a))
  BTxt (TAttDef a) -> BTxt (TAttDef (attr a))
  BTxt (TMText x) -> BTxt (TMText x { mtStyle = m (mtStyle x) })
  BPoly (PInsert i) -> BPoly (PInsert i { inName = intern t (inName i) })
  BDim (DDimension d) -> BDim (DDimension d { dmGeometry = m (dmGeometry d), dmStyle = m (dmStyle d) })
  BDim (DLeader l) -> BDim (DLeader l { ldStyle = m (ldStyle l) })
  BTab (TLayer l) -> BTab (TLayer l { lyLinetype = m (lyLinetype l) })
  _ -> b
  where
    m = internMaybe t
    td x = x { tdStyle = m (tdStyle x) }
    attr a = a { atData = td (atData a), atTag = intern t (atTag a) }

internClass :: Interner -> Class -> Class
internClass t c = c
  { clsDxfName = intern t (clsDxfName c), clsCppName = intern t (clsCppName c), clsApp = intern t (clsApp c) }
