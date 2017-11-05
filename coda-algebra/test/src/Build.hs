module Build
  ( exceptions, toFile, files
  ) where

import Build_doctests
import Data.List (notElem)

files :: [FilePath]
files = map toFile (filter (`notElem` exceptions) module_sources)

exceptions :: [String]
exceptions =
  [ "Coda.Console.Unicode"
  , "Paths_coda"
  ]

toFile :: String -> String
toFile xs = "src/" ++ map (\c -> if c == '.' then '/' else c) xs ++ ".hs"
