{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Api
  ( brewAPIFromConfig
  ) where

import Config (AppT(..), Config(..))
import Control.Monad.Except
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class
import Control.Natural
import Data.Int (Int64)

import Network.Wai (Application)
import Servant

import Api.Brewer

brewAPI :: Proxy BrewAPI
brewAPI = Proxy

-- The main type of our application
-- combining multiple sub-APIs together
type BrewAPI = BrewerAPI :<|> Raw

-- | Finally, this function takes a configuration and runs our 'UserAPI'
-- alongside the 'Raw' endpoint that serves all of our files.
brewAPIFromConfig :: Config -> Application
brewAPIFromConfig config = serve brewAPI (brewerServer :<|> files)
  where
    brewerServer = brewerServerFromConfig config

files :: Server Raw
files = serveDirectory "assets"

convertApp :: Config -> AppT IO a -> Handler a
convertApp config appt = Handler $ runReaderT (runApp appt) config

brewerServerFromConfig :: Config -> Server BrewerAPI
brewerServerFromConfig config =
  hoistServer brewerAPI (convertApp config) brewerServer
