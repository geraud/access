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

loadInstancesMetaData :: Account              -- ^ Account settings for a region
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
    M.fromList [ ("architecture", toText $ i ^.i1Architecture)
               , ("availibility_zone", fromMaybe "" $ i ^.i1Placement.pAvailabilityZone)
               , ("hypervisor_type", toText $ i ^.i1Hypervisor)
               , ("image_id", i ^. i1ImageId)
               , ("instance_id", i ^. i1InstanceId)
               , ("instance_type", toText $ i ^.i1InstanceType)
               , ("private_dns", toValue $ i ^.i1PrivateDnsName)
               , ("private_ip", toValue $ i ^.i1PrivateIpAddress)
               , ("public_dns", toValue $ i ^.i1PublicDnsName)
               , ("public_ip", toValue $ i ^.i1PublicIpAddress)
               , ("region", T.init . fromMaybe "x" $ i ^.i1Placement.pAvailabilityZone)
               , ("state", toText $ i ^.i1State.isName)
               , ("virtualization_type", toText $ i ^.i1VirtualizationType)
               , ("vpc_id", toValue $ i ^.i1VpcId)
               ]
  where toValue = fromMaybe "(no data)"

metaDataFromTags :: [Tag]            -- ^ list of tags to process
                 -> InstanceMetaData -- ^ InstanceMetaData built from the tags
metaDataFromTags ts = M.fromList $ (first T.toLower).(\t -> (t^.tagKey, t^.tagValue)) <$> ts
