{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Models.Database where

import Control.Monad.Reader
import Data.Aeson (FromJSON, ToJSON)
import Database.Persist.Sql
import Database.Persist.TH
  ( mkMigrate
  , mkPersist
  , persistLowerCase
  , share
  , sqlSettings
  )
import GHC.Generics (Generic)

import Config

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Brewer json sql=brewers
    name String
    email String
    UniqueBrewerEmail email
    deriving Show Read
Brew json sql=brews
    name String
    brewer BrewerId
    predecessors BrewId List
    deriving Show Read
Reviewer json sql=reviewers
    nickname String Maybe
    email String
    UniqueReviewerEmail email
    deriving Show Read
Review json sql=reviews
    title String
    description String
    reviewer String
    rating Int
    UniqueReviewer reviewer
    deriving Show Read
|]

performMigrationsWithPool :: ConnectionPool -> IO ()
performMigrationsWithPool = runSqlPool (runMigration migrateAll)

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
  pool <- asks getPool
  liftIO $ runSqlPool query pool
