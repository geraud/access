{-# LANGUAGE OverloadedStrings #-}
module Access.Config
    ( Configuration
    , Account (..)
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

loadConfiguration :: IO Configuration -- loads the configuration from $HOME/.accessrc
loadConfiguration = do
    configurationFile <- (</> ".accessrc") <$> getHomeDirectory
    cfg <- load [Required configurationFile]
    decodeConfiguration cfg

decodeConfiguration :: Config
                    -> IO Configuration
decodeConfiguration cfg = do
    accountNames <- require cfg "access.accounts" :: IO [Text]
    cfgAccounts <- concat <$> mapM (decodeAccountConfiguration cfg) accountNames
    cfgFields <- lookupDefault ["instance_id"] cfg "access.fields"
    cfgSort <- lookupDefault [] cfg "access.sort"
    cfgCommand <- lookupDefault "ssh $(public_dns)" cfg "access.command"
    return Configuration { _accounts = cfgAccounts
                         , _fields = cfgFields
                         , _sort = cfgSort
                         , _command = cfgCommand
                         }

decodeAccountConfiguration :: Config
                           -> Text
                           -> IO [Account]
decodeAccountConfiguration c n = do
    accessKeyId <- require c (n <> ".access_key_id")
    secretAccessKey <- require c (n <> ".secret_access_key")
    regions <- parseRegions <$> lookupDefault ["us-east-1"] c (n <> ".regions")
    let creds = FromKeys (AccessKey accessKeyId) (SecretKey secretAccessKey)
    mapM (\r -> getEnv r creds >>= \e -> return $ Account n e) regions

parseRegions :: [Text]
             -> [Region]
parseRegions regions = rights $ fromText <$> regions

{-
dumpDefaultSettings :: IO ()
dumpDefaultSettings = putStrLn . unlines defaultRC
  where
    defaultRC = [ "access {"
                , "   accounts = [\"default\"]"
                , "   display = [\"name\"]"
                , "   sort = [\"account\", \"name\"]"
                , "   ssh = [\"ssh\", \"-A\",\"{name}\"]"
                , "}"
                , ""
                , "default {"
                , "  access_key_id = \"AK....\""
                , "  secret_access_key= \"Al...\""
                , "  regions = [\"us-east-1\", \"us-west-2\"]"
                , "}"
                ]
-}
