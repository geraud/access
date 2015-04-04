{-# LANGUAGE OverloadedStrings #-}
module Access.Execution where

import           Control.Arrow      (first, (***))
import           Data.Char          (isAlphaNum, toUpper)
import qualified Data.Map.Strict    as M
import           Data.Monoid        ((<>))
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import           System.Environment (getEnvironment)
import           System.Exit        (exitWith)
import           System.Process     (CreateProcess (..), StdStream (..),
                                     createProcess, shell, waitForProcess)

import           Access.Types

executeCommand :: Text
               -> [InstanceMetaData]
               -> IO ()
executeCommand _ [] = error "No matches found."
executeCommand cmd (imd:_) = do
    e <- getEnvironment
    let processEnv = e <> toEnvironment imd
        name = imd M.! "name"
        region = imd M.! "region"
        public_dns = imd M.! "public_dns"
        message = "Connecting " <> name <> " in " <> region <> " (" <> public_dns <> ")"
        cp = (shell $ T.unpack cmd) { std_in = Inherit
                                    , std_out = Inherit
                                    , std_err= Inherit
                                    , env = Just processEnv
                                    }
    T.putStrLn message
    (_, _, _, processHandle) <- createProcess cp
    rc <- waitForProcess processHandle
    exitWith rc

-- | Convert the insteance meta-data hash to a list.
toEnvironment :: InstanceMetaData   -- ^ instance meta-data
              -> [(String, String)] -- ^ list of key/value tuples
toEnvironment imd = first capitalizeKey . T.unpack *** T.unpack <$> M.toList imd

-- | The keys are capitalized and are purged of non
-- alpha-numeric character by converting them to underscores.
capitalizeKey :: String -- ^ input such as aws:cloudformation:stack-name
              -> String -- ^ ouptut AWS_CLOUDFORMATION_STACK_NAME
capitalizeKey key = (\c -> if isAlphaNum c then toUpper c else '_') <$> key
