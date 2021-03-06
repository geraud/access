{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Access.Types where

import qualified Data.Map.Strict as M
import           Data.Text       (Text)
import           Network.AWS     (Env)

type InstanceMetaData = M.Map Text Text

data Account = Account
    { getAccountName :: Text -- name of the account
    , getAccountEnv  :: Env  -- aws environment (combination of the credentials and the region)
    }

data Configuration = Configuration
    { getAccounts   :: [Account]
    , getFields     :: [Text]
    , getSortFields :: [Text]
    , getCommand    :: Text
    }

data Predicate
    = KeyValueMatcher Text Text
    | ValueMatcher Text
