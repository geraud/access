{-# LANGUAGE OverloadedStrings #-}
module Access.Config
    ( Configuration
    , Account (..)
    , loadConfiguration
    ) where

import           Data.Configurator
import           Data.Configurator.Types (Config)
import           Data.Either             (rights)
import           Data.Monoid             ((<>))
import           Data.Text               (Text)
import           Network.AWS             (AccessKey (..), Credentials (..),
                                          SecretKey (..), getEnv)
import           Network.AWS.Data        (fromText)
import           System.Directory
import           System.FilePath.Posix

import           Access.Types

loadConfiguration :: IO Configuration -- loads the configuration from $HOME/.accessrc
loadConfiguration = do
    configurationFile <- (</> ".accessrc") <$> getHomeDirectory
    cfg <- load [Required configurationFile]
    decodeConfiguration cfg

-- | Extracts data out of the Config and create a Configuration record.
decodeConfiguration :: Config           -- ^ configurator's Config
                    -> IO Configuration -- ^
decodeConfiguration cfg = do
    accountNames <- require cfg "access.accounts" :: IO [Text]
    cfgAccounts <- concat <$> mapM (decodeAccountConfiguration cfg) accountNames
    cfgFields <- lookupDefault ["instance_id"] cfg "access.fields"
    cfgSortFields <- lookupDefault [] cfg "access.sort_by"
    cfgCommand <- lookupDefault "ssh $(public_dns)" cfg "access.command"
    return Configuration { _accounts = cfgAccounts
                         , _fields = cfgFields
                         , _sortFields = cfgSortFields
                         , _command = cfgCommand
                         }

-- | Decode the data necessary for querying AWS
decodeAccountConfiguration :: Config       -- ^ Config containing the account's data
                           -> Text         -- ^ name of the account
                           -> IO [Account] -- ^ return an Account record for each region found
decodeAccountConfiguration c n = do
    accessKeyId <- require c (n <> ".access_key_id")
    secretAccessKey <- require c (n <> ".secret_access_key")
    regions <- parseRegions <$> lookupDefault ["us-east-1"] c (n <> ".regions")
    let creds = FromKeys (AccessKey accessKeyId) (SecretKey secretAccessKey)
    mapM (\r -> getEnv r creds >>= \e -> return $ Account n e) regions
  where
    parseRegions regions = rights $ fromText <$> regions
