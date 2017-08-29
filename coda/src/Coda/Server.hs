--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2017
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Coda.Server
  ( server
  ) where

import Coda.Server.Options
import Coda.Message.Builder
import Coda.Message.Sink
-- import Coda.Util.Async
import Control.Concurrent.MVar
import Control.Lens
import Control.Logging
import Data.Aeson
import Data.ByteString.Lazy as Lazy
import Data.String
import Data.Text

withLogging :: ServerOptions -> IO () -> IO ()
withLogging opts m = case opts^.serverOptionsLog of
  Nothing -> withStderrLogging m
  Just f -> withFileLogging f m

jsonRpc :: Text
jsonRpc = fromString "JSON-RPC"

withMVar_ :: MVar a -> IO b -> IO b
withMVar_ v m = withMVar v (const m)

server :: ServerOptions -> IO ()
server opts = withLogging opts $ do
  setLogLevel $ if opts^.serverOptionsDebug then LevelDebug else LevelWarn
  outputLock <- newMVar ()
  let output :: Sink Encoding
      output = Sink (withMVar_ outputLock . putEncoding)
  _input <- Lazy.getContents
  loggingLogger LevelError jsonRpc $ show opts
  sink output $ toEncoding Null

{-
  -- now that we've got a place to write to and a supply of messages

  handle input output
 where
  handle :: Lazy.ByteString -> Sink Encoding -> IO ()
  handle input output = case runParser parseRpc input of
    Err -> do
      loggingLogger LevelError JsonRpc "bad frame"
      exit $ ExitFailure 1 -- we're shutting down
   OK value input' -> do
      loggingLogger LevelInfo JsonRpc (show value)
      case value of
        Array xs -> do
          batch xs output
          handle input' output
        _ -> process value output

processRequest :: Request Id Value -> Sink (Response Id Value) -> IO (Async ())

process :: Value -> Sink Encoding -> IO ()
process value sink = case eitherDecode' value of
  Left err -> do
    loggingLogger LevelWarning JsonRpc err
    output
  Right (Request Nothing m p   -> processNote (Notification m p)
  Right (Request mi m p) -> do
    e <- newEmptyMVar
    a <- processRequest (Request i m p) $ Sink $ \response ->
      if response^.id == i then putMVar () >> sink (toEncoding xs)
      else loggingLogger LevelWarn JsonRpc "bad response"
    concurrently a $ do


   : Request (Maybe Id) Value) -> do
    requestId req
            handle input' output
          Right req -> do
            dispatch req output

batch :: Vector Value -> Sink Encoding -> IO ()
batch xs =

dispatch :: Request_ -> Sink Encoding -> IO ()
dispatch request out = do

notification :: Notification_ -> Sink Encoding -> IO ()
-}
