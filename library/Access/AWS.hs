{-# LANGUAGE OverloadedStrings #-}
module Access.AWS
    ( loadInstancesMetaData
    , InstanceMetaData
    ) where

import           Control.Arrow           (first)
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

loadInstancesMetaData :: Account               -- ^ Account settings for a region
                      -> IO [InstanceMetaData] -- ^ list of all InstanceMetaData found for each instance in this account
loadInstancesMetaData (Account name env) = do
    result <- runAWST env $ send describeInstances
    case result of
        Left errorMsg -> do
            putStrLn $ "Error: " <> show errorMsg
            return []
        Right result' -> do
            let imds = processInstance <$> (result' ^..dirReservations.traverse.rInstances.traverse)
            return $ M.insert "account" name <$> imds

-- | processInstance transforms an AWS record
processInstance :: Instance         -- ^ An AWS instance record
                -> InstanceMetaData -- ^ InstanceMetaData combining data from the instance and its tags
processInstance i = let mInst = metaDataFromInstance i
                        mTags = metaDataFromTags (i^.i1Tags)
                    in mInst <> mTags

metaDataFromInstance :: Instance         -- ^ An AWS instance record
                     -> InstanceMetaData -- ^ InstanceMetaData extracted from the instance
metaDataFromInstance i =
    M.fromList [ ("architecture", i ^.i1Architecture & toText)
               , ("availability_zone", i ^.i1Placement.pAvailabilityZone & toValue)
               , ("hypervisor_type", i ^.i1Hypervisor & toText)
               , ("image_id", i ^. i1ImageId)
               , ("instance_id", i ^. i1InstanceId)
               , ("instance_type", i ^.i1InstanceType & toText)
               , ("private_dns", i ^.i1PrivateDnsName & toValue)
               , ("private_ip", i ^.i1PrivateIpAddress & toValue)
               , ("public_dns", i ^.i1PublicDnsName & toValue)
               , ("public_ip", i ^.i1PublicIpAddress & toValue)
               , ("region", i ^.i1Placement.pAvailabilityZone & toRegion)
               , ("state", i ^.i1State.isName & toText)
               , ("virtualization_type", i ^.i1VirtualizationType & toText)
               , ("vpc_id", i ^.i1VpcId & toValue)
               ]
  where toValue = fromMaybe "(no data)"
        toRegion = T.init . fromMaybe "x"

metaDataFromTags :: [Tag]            -- ^ list of tags to process
                 -> InstanceMetaData -- ^ InstanceMetaData built from the tags
metaDataFromTags ts = M.fromList $ (first T.toLower) . (\t -> (t^.tagKey, t^.tagValue)) <$> ts
