{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2017, (c) Edward Kmett and Dan Doel 2012-2013
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Coda.Console.Command
  ( Command(..)
  , HasCommand(..)
  , commands
  , executeCommand
  ) where

import Coda.Util.Pretty
import Coda.Version
import Coda.Syntax.Dyck
import Coda.Syntax.Lexer
import Control.Lens as Lens
import Control.Monad.IO.Class
import Data.Char
import Data.List as List
import Data.List.Split (splitOn)
import Data.Monoid
import Data.String
import System.Console.Haskeline
import System.Exit
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>))

------------------------------------------------------------------------------
-- Command
------------------------------------------------------------------------------

data Command = Command
  { _cmdName :: String
  , _alts    :: [String]
  , _arg     :: Maybe String
  , _tabbed  :: Maybe (CompletionFunc IO)
  , _desc    :: String
  , _body    :: [String] -> String -> IO ()
  }

makeClassy ''Command

cmd :: String -> Command
cmd nm = Command nm [] Nothing Nothing "" $ \_ _ -> return ()

getCommand :: String -> Maybe (Command, [String], String)
getCommand zs = commands ^?
    folded.
    filtered (\c -> isPrefixOf xs (c^.cmdName)
                 || anyOf (alts.folded) (isPrefixOf xs) c).
    to (,as,ys')
  where
    (cs, ys) = break isSpace zs
    xs:as = splitOn "+" cs
    ys' = reverse $ dropWhile isSpace $ reverse $ dropWhile isSpace ys

executeCommand :: String -> IO ()
executeCommand txt = case getCommand txt of
  Just (c,args,input)  -> view body c args input
  Nothing          -> do
    sayLn $ red (text "Unknown command") <+> bold (text (show (cons ':' txt)))
    sayLn $ text "Use" <+> bold (text (show ":?")) <+> "for help."

showHelp :: [String] -> String -> IO ()
showHelp _ _ = sayLn $ vsep (map format commands) where
  format c = fill 18 (withArg c) <+> hang 18 (fillSep (text <$> words (c^.desc)))
  withArg c = case c^.arg of
    Nothing -> bold (char ':' <> text (c^.cmdName))
    Just a  -> bold (char ':' <> text (c^.cmdName)) <+> angles (text a)

commands :: [Command]
commands =
  [ cmd "help" & desc .~ "show help" & alts .~ ["?"] & body .~ showHelp
  , cmd "quit" & desc .~ "quit" & body.mapped .~ const (liftIO exitSuccess)
  , cmd "dyck" & desc .~ "debug dyck language tokenization" & body.mapped .~ \input ->
       liftIO $ putStrLn $ show (fromString input :: Dyck Token)
  , cmd "version"
      & desc .~ "show the compiler version number"
      & body .~ \_ _ -> liftIO $ putStrLn version
  ]
