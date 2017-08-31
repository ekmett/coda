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
  , showMessage
  ) where


import Coda.Server.Options
import Coda.Util.Aeson
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Reader
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.ByteString.Lazy as Lazy
import Data.Monoid
import Data.Text as Text
import Language.Server.Base
import Language.Server.Builder
import Language.Server.Protocol
import Language.Server.Parser
import Language.Server.Severity
import System.Exit
import System.IO

--------------------------------------------------------------------------------
-- Logging
--------------------------------------------------------------------------------

putError :: MonadIO m => Maybe Id -> ErrorCode -> Text -> m ()
putError i c t = -- do
  putMessage $ Response i Nothing (Just (ResponseError c t Nothing))
--  liftIO $ loggingLogger LevelError jsonRpc text

--------------------------------------------------------------------------------
-- State
--------------------------------------------------------------------------------

data ServerState = ServerState
  { _input :: !Lazy.ByteString
  , _shutdownRequested :: !Bool
  } deriving (Show)

makeClassy ''ServerState

--------------------------------------------------------------------------------
-- Listening
--------------------------------------------------------------------------------

listen :: (MonadIO m, MonadState s m, HasServerState s) => m (Either [Request] Request)
listen = use input >>= \i -> case runParser parseMessage i of
  Err -> do
    liftIO $ hPutStrLn stderr "Bad..."
    putError Nothing InvalidRequest "Unparseable JSON-RPC Request Frame Received. Shutting Down."
    liftIO $ exitWith $ ExitFailure 1
  OK value i' -> do
    liftIO $ hPutStrLn stderr "<"
    liftIO $ Lazy.hPutStr stderr value
    liftIO $ hPutStrLn stderr ">"
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
server opts = do
  liftIO $ hSetBuffering stdin NoBuffering
  liftIO $ hSetEncoding stdin utf8
  c <- Lazy.getContents
  runReaderT ?? opts $ evalStateT ?? ServerState c False $ do
    liftIO $ hPutStrLn stderr "Starting!"

    liftIO $ hSetBuffering stdout NoBuffering
    liftIO $ hSetEncoding stdout utf8
    liftIO $ hFlush stdout

    liftIO $ hSetBuffering stderr NoBuffering
    liftIO $ hSetEncoding stderr utf8
    liftIO $ hFlush stderr

    liftIO $ hPutStrLn stderr "Initializing!"
    initializeServer
    liftIO $ hPutStrLn stderr "Initialized..."
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
  Right (Request Nothing "shutdown" Nothing) -> do
    assign shutdownRequested True
    initializeServer
  Right (Request Nothing "exit" Nothing) -> 
    use shutdownRequested >>= \b -> liftIO $ exitWith $ if b then ExitSuccess else ExitFailure 1
  Right (Request _ m _) 
    | Text.isPrefixOf "$/" m -> initializeServer -- ignore extensions
  Right (Request Nothing _ _) -> initializeServer               -- ignore notifications
  Right (Request (Just i) _ _) -> do
    putError (Just i) ServerNotInitialized "waiting for initialization"
    initializeServer 
  Left _ -> do
    putError Nothing InternalError "batch commands not yet implemented"
    initializeServer

loop :: (MonadState s m, HasServerState s, MonadReader e m, HasServerOptions e, MonadIO m) => m ()
loop = listen >>= \case
  Right (Request Nothing "shutdown" Nothing) -> loop
  Right (Request Nothing "exit" Nothing) -> 
    use shutdownRequested >>= \b -> liftIO $ exitWith $ if b then ExitSuccess else ExitFailure 1
  Right (Request _ m _) | Text.isPrefixOf "$/" m -> loop -- ignore extensions for now
  Right (Request (Just i) _ _) -> do
    putError (Just i) InvalidRequest "unsupported request"
    loop
  Right (Request _ m _) -> logMessage Information m
  Left _ -> do
    putError Nothing InternalError "batch commands not yet implemented"
    loop
