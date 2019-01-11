{-# language GADTs #-}

module Syntax.Parser where

import Syntax.Dyck

data Parsed = Parsed
  { parsedNone :: ()
  , parsedDo :: Bool
  }

data Tag a where
  None :: Tag ()
  Do   :: Tag Bool

parseNone :: Dyck -> ()
parseNone _ = ()

parseDo :: Dyck -> Bool
parseDo _ = False

parse :: Dyck -> Parsed
parse d = Parsed (parseNone d) (parseDo d)

retrieve :: Parsed -> Tag a -> a
retrieve d None = parsedNone d
retrieve d Do   = parsedDo d
