module Summary
  ( Summary
  , summarize
  , mergeSummary
  ) where

import Data.Text

import Relative.Delta.Type

import Dyck

type Summary = ()

summarize :: Text -> Dyck -> Summary
summarize _ _ = ()

mergeSummary :: Int -> Delta -> Summary -> Int -> Delta -> Summary -> Summary
mergeSummary _ _ _ _ _ _ = ()
