{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Access.Types where

import           Control.Lens
import qualified Data.Map.Strict as M
import           Data.Monoid     ((<>))
import           Data.Text       (Text)
import           Network.AWS     (Env, envRegion)

type InstanceMetaData = M.Map Text Text

data Account = Account
    { _accountName :: Text -- name of the account
    , _accountEnv  :: Env  -- aws environment (combination of the credentials and the region)
    }

makeLenses ''Account

instance Show Account where
    show a = "Account " <> show (a ^. accountName) <> " " <> show (a ^. accountEnv.envRegion)

data Configuration = Configuration
    { _accounts :: [Account]
    , _fields   :: [Text]
    , _sort     :: [Text]
    , _command  :: Text
    }

makeLenses ''Configuration

data Predicate
    = KeyValueMatcher Text Text
    | ValueMatcher Text
