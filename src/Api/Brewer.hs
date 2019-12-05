{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Brewer where

import Config (AppT, Config)
import Control.Monad.Reader
import Database.Persist.Postgresql
  ( Entity(..)
  , (==.)
  , get
  , insert
  , selectFirst
  , selectList
  , toSqlKey
  )
import Models.Database
import Servant

type BrewerAPI = GetBrewersRoute :<|> GetBrewerRoute :<|> PostBrewerRoute

type GetBrewersRoute = "brewers" :> Get '[ JSON] [Entity Brewer]

type GetBrewerRoute
   = "brewers" :> Capture "brewerId" Int :> Get '[ JSON] (Maybe (Entity Brewer))

type PostBrewerRoute
   = "brewers" :> ReqBody '[ JSON] Brewer :> Post '[ JSON] (Maybe (Entity Brewer))

brewerAPI :: Proxy BrewerAPI
brewerAPI = Proxy

brewerServer :: (MonadIO m) => ServerT BrewerAPI (AppT m)
brewerServer = allBrewers :<|> singleBrewer :<|> createBrewer

allBrewers :: (MonadIO m) => AppT m [Entity Brewer]
allBrewers = runDb (selectList [] [])

singleBrewer :: (MonadIO m) => Int -> AppT m (Maybe (Entity Brewer))
singleBrewer identifier = runDb (selectFirst [BrewerId ==. key] [])
  where
    key = toSqlKey $ fromIntegral identifier

createBrewer :: (MonadIO m) => Brewer -> AppT m (Maybe (Entity Brewer))
createBrewer b = do
  key <- runDb (insert (Brewer (brewerName b) (brewerEmail b)))
  v <- runDb (get key)
  return $ fmap (Entity key) v
