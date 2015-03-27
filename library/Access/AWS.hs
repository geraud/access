{-# LANGUAGE OverloadedStrings #-}
module Access.AWS
    ( loadInstanceData
    , InstanceMetaData
    ) where

import           Control.Lens
import           Control.Monad.Trans.AWS
import qualified Data.Map.Strict         as M
import           Data.Maybe              (fromMaybe)
import           Data.Monoid             ((<>))
import qualified Data.Text               as T
import           Network.AWS.Data        (toText)
import           Network.AWS.EC2
import           Prelude                 hiding (error)

import           Access.Types

loadInstanceData :: Account
                 -> IO [InstanceMetaData]
loadInstanceData (Account name env) = do
    result <- runAWST env $ send describeInstances
    case result of
        Left errorMsg -> do
            putStrLn $ "Error: " <> show errorMsg
            return []
        Right result' -> do
            let imds = concatMap processReservation (result'^.dirReservations)
            return $ (M.insert "account" name) <$> imds

processReservation :: Reservation
                   -> [InstanceMetaData]
processReservation r = processInstance <$> (r^.rInstances)

processInstance :: Instance
                -> InstanceMetaData
processInstance i =
    let mInst = metaDataFromInstance i
        mTags = metaDataFromTags (i^.i1Tags)
    in mInst <> mTags

metaDataFromInstance :: Instance
                     -> InstanceMetaData
metaDataFromInstance i =
    M.fromList [ ("availibility_zone", fromMaybe "" $ i ^. i1Placement.pAvailabilityZone)
               , ("instance_id", i ^. i1InstanceId)
               , ("instance_type", toText $ i ^. i1InstanceType)
               , ("private_ip", fromMaybe "" $ i ^. i1PrivateIpAddress)
               , ("public_dns", fromMaybe "" $ i ^. i1PublicDnsName)
               , ("public_ip", fromMaybe "" $ i ^. i1PublicIpAddress)
               , ("region", T.init . (fromMaybe "x") $ i ^. i1Placement.pAvailabilityZone)
               , ("state", toText $ i ^. i1State.isName)
               ]

metaDataFromTags :: [Tag]
                 -> InstanceMetaData
metaDataFromTags ts = M.fromList $ convertTag <$> ts
  where convertTag t = ((T.toLower $ t^.tagKey), (t^.tagValue))
