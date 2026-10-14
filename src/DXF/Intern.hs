{-# LANGUAGE BangPatterns #-}
-- | String interning: a document names a few distinct layers, linetypes,
-- styles and blocks on thousands of entities.  An 'Interner' maps the
-- bytes of such a string to the one copy that every entity then holds, so
-- that none keeps a slice of the input alive and equal strings are the
-- same bytes in memory ('==' on 'B.ByteString' tries the pointer first).
--
-- One table is made per parse ('withInterner') and used by the fast
-- decoders and by the general path alike.  It is a membudget interner,
-- named @dxf.Intern@ in membudget's registry, with a budget of 20,000
-- strings (dxf's cap before membudget); over it a string is itself.  The
-- budget can be set for a run with @DXF_INTERN_BUDGET@ (@e300@: entries,
-- @b65536@: bytes), to check that a bounded table gives the same output.
-- 'intern' is pure -- interning does not change a value, so the order in
-- which strings reach the table does not matter.
module DXF.Intern
  ( Interner, withInterner, intern, internMaybe, internSize, internBudget
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import MemBudget (global)
import MemBudget.Budget (Budget, bytes, entries)
import qualified MemBudget.Intern as MI
import System.Environment (lookupEnv)

type Interner = MI.Interner B.ByteString

-- | The table's budget: 20,000 strings, or @DXF_INTERN_BUDGET@.
internBudget :: Budget
internBudget = global (maybe (entries 20000) parse <$> lookupEnv "DXF_INTERN_BUDGET")
  where
    parse s = case BC.uncons (BC.pack s) of
      Just ('b', n) -> bytes (number n)
      Just ('e', n) -> entries (number n)
      _ -> entries (number (BC.pack s))
    number n = maybe 20000 fst (BC.readInt n)
{-# NOINLINE internBudget #-}

-- | A value made with a fresh table (one parse's).
withInterner :: (Interner -> a) -> a
withInterner = MI.withInterner internBudget "dxf.Intern"
{-# INLINE withInterner #-}

-- | The distinct strings held.
internSize :: Interner -> IO Int
internSize = MI.internSize

maxLen :: Int
maxLen = 64

-- | The one copy of a string (the string itself when it is empty, longer
-- than 64 bytes, or over the budget).
intern :: Interner -> B.ByteString -> B.ByteString
intern t v
  | n == 0 || n > maxLen = v
  | otherwise = MI.intern t v
  where !n = B.length v
{-# INLINE intern #-}

internMaybe :: Interner -> Maybe B.ByteString -> Maybe B.ByteString
internMaybe t (Just v) = Just $! intern t v
internMaybe _ Nothing = Nothing
{-# INLINE internMaybe #-}
