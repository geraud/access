{-# LANGUAGE OverloadedStrings #-}
module Access.Config
    ( Configuration
    , Deployment (..)
    , loadConfiguration
    ) where

import           Control.Applicative     ((<$>))
import           Data.Configurator
import           Data.Configurator.Types (Config)
import           Data.Either             (rights)
import           Data.Monoid             ((<>))
import           Data.Text               (Text)
import           Network.AWS             (AccessKey (..), Credentials (..),
                                          SecretKey (..))
import           Network.AWS             (Region, getEnv)
import           Network.AWS.Data        (fromText)
import           System.Directory
import           System.FilePath.Posix

import           Access.Types

loadConfiguration :: IO Configuration -- loads the configuration from $HOME/etc/access.cfg
loadConfiguration = do
    home <- getHomeDirectory
    let configurationFile =  home </> "etc" </> "access.cfg"
    cfg <- load [Required configurationFile]
    decodeConfiguration cfg

decodeConfiguration :: Config
                    -> IO Configuration
decodeConfiguration cfg = do
    deployments <- require cfg "access.deployments"
    concat <$> mapM (decodeDeploymentConfiguration cfg) deployments

decodeDeploymentConfiguration :: Config
                              -> Text
                              -> IO Configuration
decodeDeploymentConfiguration c n = do
    regions <- parseRegions <$> lookupDefault ["us-east-1"] c (n <> ".regions")
    accessKeyId <- require c (n <> ".access_key_id")
    secretAccessKey <- require c (n <> ".secret_access_key")
    let creds = FromKeys (AccessKey accessKeyId) (SecretKey secretAccessKey)
    mapM (\r -> getEnv r creds >>= \e -> return $ Deployment n e) regions

parseRegions :: [Text]
             -> [Region]
parseRegions regions = rights $ fmap fromText regions
