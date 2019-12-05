{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( startApp
  ) where

import Api.Api (brewAPIFromConfig)
import Config (Config(..), Environment(..), makePool, setLogger)
import Control.Monad.Reader
import Models.Database (performMigrationsWithPool)
import Network.Wai.Handler.Warp (run)
import Safe (readMay)
import System.Environment (lookupEnv)

startApp :: IO ()
startApp = do
  env <- lookupSetting "ENV" Development
  runport <- lookupSetting "PORT" 1234
  pool <- makePool env
  putStrLn $ "Performing migrations on" ++ show pool
  performMigrationsWithPool pool
  let cfg = Config {getPool = pool, getEnv = env}
      logger = setLogger env
  putStrLn $ "Environment: " ++ show env
  putStrLn $ "Port: " ++ show runport
  run runport . logger . brewAPIFromConfig $ cfg

lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
  maybeValue <- lookupEnv env
  case maybeValue of
    Nothing -> return def
    Just str -> maybe (handleFailedRead str) return (readMay str)
  where
    handleFailedRead str =
      error $
      mconcat ["Failed to read [[", str, "]] for environment variable ", env]
