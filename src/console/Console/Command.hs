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

module Console.Command
  ( Command(..)
  , HasCommand(..)
  , commands
  , executeCommand
  ) where

import Control.Lens as Lens
import Control.Monad.IO.Class
import Data.Char
import Data.List as List
import Data.List.Split (splitOn)
import Data.Monoid
import Data.String
import Data.Text (pack)
import System.Console.Haskeline
import System.Exit
-- import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>))
import Data.Text.Prettyprint.Doc
-- import Data.Text.Prettyprint.Doc.Render.Text as RenderText
import Data.Text.Prettyprint.Doc.Render.Terminal as RenderTerminal
import Prelude hiding (lex)

import Console.Pretty
import Syntax.Dyck
import Syntax.Lexer
import Version

------------------------------------------------------------------------------
-- Command
------------------------------------------------------------------------------

data Command = Command
  { _cmdName :: String
  , _alts    :: [String]
  , _arg     :: Maybe String
  , _tabbed  :: Maybe (CompletionFunc IO)
  , _desc    :: String
  , _body    :: FancyOptions -> [String] -> String -> IO ()
  }

makeClassy ''Command

cmd :: String -> Command
cmd nm = Command nm [] Nothing Nothing "" $ \_ _ _ -> return ()

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

executeCommand :: FancyOptions -> String -> IO ()
executeCommand fancy txt = case getCommand txt of
  Just (c,args,input)  -> view body c fancy args input
  Nothing          -> do
    putFancy fancy $ annotate (color Red) "Unknown command" <+> annotate bold (pretty (cons ':' txt))
    putFancy fancy $ "Use" <+> annotate bold (pretty ":?") <+> "for help."

showHelp :: FancyOptions -> [String] -> String -> IO ()
showHelp fancy _ _ = putFancy fancy $ vsep (format <$> commands) where
  format c = fill 18 (withArg c) <+> hang 18 (fillSep (pretty <$> words (c^.desc)))
  withArg c = case c^.arg of
    Nothing -> annotate bold (pretty ':' <> pretty (c^.cmdName))
    Just a  -> annotate bold (pretty ':' <> pretty (c^.cmdName)) <+> angles (pretty a)

commands :: [Command]
commands =
  [ cmd "help" & desc .~ "show help" & alts .~ ["?"] & body .~ showHelp
  , cmd "quit" & desc .~ "quit" & body .~ \_ _ _ -> liftIO exitSuccess
  , cmd "dyck" & desc .~ "debug dyck language tokenization" & body .~ \_ _ input ->
       liftIO $ print (lex (pack input) :: Dyck)
  , cmd "version"
      & desc .~ "show the compiler version number"
      & body .~ \_ _ _ -> liftIO $ putStrLn version
  ]
