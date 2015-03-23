module Access.Types
    ( InstanceMetaData
    , Deployment (..)
    , Configuration
    ) where

import qualified Data.Map.Strict as M
import           Data.Text       (Text)
import           Network.AWS     (Env)

type InstanceMetaData = M.Map Text Text

data Deployment = Deployment Text Env

type Configuration = [Deployment]
