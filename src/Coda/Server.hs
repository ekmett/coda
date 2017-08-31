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
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Reader
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.ByteString.Lazy as Lazy
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
  { _shutdownRequested :: !Bool
  } deriving (Show)

makeClassy ''ServerState

--------------------------------------------------------------------------------
-- Listening
--------------------------------------------------------------------------------

eitherDecodeRequest :: Lazy.ByteString -> Either String (Either [Request] Request)
eitherDecodeRequest bs
    = Left  <$> eitherDecode' bs
  <|> Right <$> eitherDecode' bs

listen :: (MonadIO m, MonadState s m, HasServerState s) => m (Either [Request] Request)
listen = liftIO (parse parseMessage stdin) >>= \case
  Left e -> do
    putError Nothing InvalidRequest (Text.pack e)
    liftIO $ exitWith $ ExitFailure 1
  Right value -> case eitherDecodeRequest value of
    Left s -> do
      putError Nothing ParseError (Text.pack s)
      listen
    Right e -> return e

--------------------------------------------------------------------------------
-- Server -> Client Notifications
--------------------------------------------------------------------------------

logMessage :: MonadIO m => Severity -> Text -> m ()
logMessage s t = putMessage $ LogMessage s t

showMessage :: MonadIO m => Severity -> Text -> m ()
showMessage s t = putMessage $ ShowMessage s t

telemetryEvent :: MonadIO m => Value -> m ()
telemetryEvent v = liftIO $ putMessage $ TelemetryEvent v

--------------------------------------------------------------------------------
-- Server
--------------------------------------------------------------------------------

server :: ServerOptions -> IO ()
server opts = do
  hSetBuffering stdin NoBuffering
  hSetEncoding stdin char8
  hSetBuffering stdout NoBuffering
  hSetEncoding stdout char8
  hFlush stdout
  runReaderT ?? opts $ evalStateT ?? ServerState False $ do
    initializeServer
    loop

ok :: (MonadIO m, ToJSON a) => Id -> a -> m ()
ok i p = liftIO $ putMessage $ Response (Just i) (Just (toJSON p)) Nothing

initializeServer :: (MonadState s m, HasServerState s, MonadReader e m, HasServerOptions e, MonadIO m) => m ()
initializeServer = listen >>= \case
  Right (Initialize i _ip) -> do
    ok i $ object
      [ "capabilities" .= object
        [ "textDocumentSync" .= object
          [ "openClose" .= toJSON True
          , "change" .= toJSON True
          , "save" .= toJSON True
          ]
        ]
      ]
  Right Shutdown -> do
    assign shutdownRequested True
    initializeServer
  Right Exit -> 
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
  Right Initialized -> loop
  Right Shutdown -> assign shutdownRequested True >> loop
  Right Exit -> 
    use shutdownRequested >>= \b -> liftIO $ exitWith $ if b then ExitSuccess else ExitFailure 1
  Right (Request _ m _) | Text.isPrefixOf "$/" m -> loop -- ignore extensions for now
  Right (Request (Just i) _ _) -> do
    putError (Just i) InvalidRequest "unsupported request"
    loop
  Right (Request _ m _) -> do
    liftIO $ hPutStrLn stderr (show m)
    logMessage Information m
    loop
  Left _ -> do
    putError Nothing InternalError "batch commands not yet implemented"
    loop
