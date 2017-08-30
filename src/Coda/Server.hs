{-# language LambdaCase #-}
{-# language TemplateHaskell #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}

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
  , logMessage
  , telemetryEvent
  ) where

import Coda.Message.Base
import Coda.Message.Builder
import Coda.Message.Language
import Coda.Message.Parser
import Coda.Message.Severity
import Coda.Server.Options
import Coda.Util.Aeson
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Reader
import Control.Lens hiding ((.=))
import Control.Logging
import Data.Aeson
import Data.ByteString.Lazy as Lazy
import Data.Monoid
import Data.String
import Data.Text as Text
import System.Exit
import System.IO

--------------------------------------------------------------------------------
-- Logging
--------------------------------------------------------------------------------

withLogging :: ServerOptions -> IO () -> IO ()
withLogging opts m = case opts^.serverOptionsLog of
  Nothing -> withStderrLogging m
  Just f -> withFileLogging f m

jsonRpc :: Text
jsonRpc = fromString "JSON-RPC"

putError :: MonadIO m => Maybe Id -> ErrorCode -> Text -> m ()
putError i code text = do
  putMessage $ Response i Nothing (Just (ResponseError code text Nothing))
  liftIO $ loggingLogger LevelError jsonRpc text

--------------------------------------------------------------------------------
-- State
--------------------------------------------------------------------------------

data ServerState = ServerState
  { _input :: !Lazy.ByteString
  } deriving (Show)

makeClassy ''ServerState

--------------------------------------------------------------------------------
-- Listening
--------------------------------------------------------------------------------

listen :: (MonadIO m, MonadState s m, HasServerState s) => m (Either [Request] Request)
listen = do
  i <- use input
  case runParser message i of
    Err -> do
      putError Nothing InvalidRequest "Unparseable JSON-RPC Request Frame Received. Shutting Down."
      liftIO $ exitWith $ ExitFailure 1
    OK value i' -> do
      assign input i'
      let req = Left  <$> eitherDecode' value
            <|> Right <$> eitherDecode' value 
      case req of
        Left s -> do
          putError Nothing ParseError $ Text.pack s
          listen
        Right e -> return e

--------------------------------------------------------------------------------
-- Server -> Client Notifications
--------------------------------------------------------------------------------

logMessage :: MonadIO m => Severity -> Text -> m ()
logMessage s t = liftIO $ putMessage $ Request Nothing "window/logMessage" $ Just $ object $
  "type" !~ s <> "message" !~ t

showMessage :: MonadIO m => Severity -> Text -> m ()
showMessage s t = liftIO $ putMessage $ Request Nothing "window/showMessage" $ Just $ object $
  "type" !~ s <> "message" !~ t

telemetryEvent :: MonadIO m => Value -> m ()
telemetryEvent v = liftIO $ putMessage $ Request Nothing "telemetry/event" (Just v)

--------------------------------------------------------------------------------
-- Server
--------------------------------------------------------------------------------

server :: ServerOptions -> IO ()
server opts = withLogging opts $ do
  setLogLevel $ if opts^.serverOptionsDebug then LevelDebug else LevelWarn
  c <- Lazy.getContents
  runReaderT ?? opts $ evalStateT ?? ServerState c $ do
    liftIO $ hSetBuffering stdout NoBuffering
    initializeServer
    loop

ok :: (MonadIO m, ToJSON a) => Id -> a -> m ()
ok i p = liftIO $ putMessage $ Response (Just i) (Just (toJSON p)) Nothing

initializeServer :: (MonadState s m, HasServerState s, MonadReader e m, HasServerOptions e, MonadIO m) => m ()
initializeServer = listen >>= \case
  Right (Request (Just i) "initialize" (Just (JSON (_ip :: InitializeParams)))) -> do
    ok i $ object
      [ "capabilities" .= object
        [ "textDocumentSync" .= object
          [ "openClose" .= toJSON True
          , "change" .= toJSON True
          , "save" .= toJSON True
          ]
        ]
      ]
  -- Right (Request Nothing "shutdown" Nothing) -> liftIO $ pure (); -- exitWith ExitSuccess -- totally wrong
  Right (Request Nothing "exit" Nothing) -> liftIO $ exitWith ExitSuccess
  Right (Request _ m _) | Text.isPrefixOf "$/" m -> initializeServer -- ignore extensions
  Right (Request Nothing _ _) -> initializeServer               -- ignore notifications
  Right (Request (Just i) _ _) -> do
    putError (Just i) ServerNotInitialized "waiting for initialization"
    initializeServer 
  Left _ -> do
    putError Nothing InternalError "batch commands not yet implemented"
    initializeServer

loop :: (MonadState s m, HasServerState s, MonadReader e m, HasServerOptions e, MonadIO m) => m ()
loop = listen >>= \case
  Right (Request Nothing "exit" Nothing) -> liftIO $ do
  --  loggingLogger LevelInfo "shutting down as requested"
    exitWith ExitSuccess
  Right (Request _ m _) | Text.isPrefixOf "$/" m -> loop -- ignore extensions for now
  Right (Request (Just i) _ _) -> do
    putError (Just i) InvalidRequest "unsupported request"
    loop
  Right (Request _ m _) -> logMessage Information m
  Left _ -> do
    putError Nothing InternalError "batch commands not yet implemented"
    loop
