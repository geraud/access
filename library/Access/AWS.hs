{-# LANGUAGE OverloadedStrings #-}
module Access.AWS
    ( loadInstanceData
    , InstanceMetaData
    ) where

import           Control.Applicative     ((<$>))
import           Control.Lens
import           Control.Monad.Trans.AWS
import qualified Data.Map.Strict         as M
import           Data.Maybe              (fromMaybe)
import           Data.Monoid             ((<>))
import           Data.Text               (Text, toLower)
import           Network.AWS.Data        (toText)
import           Network.AWS.EC2
import           Prelude                 hiding (error)

import           Access.Types

loadInstanceData :: Deployment
                 -> IO [InstanceMetaData]
loadInstanceData (Deployment name env)= do
    result <- runAWST env $ send describeInstances
    case result of
        Left errorMsg -> do
            putStrLn $ "Error: " <> show errorMsg
            return []
        Right result' -> do
            let imds = concatMap processReservation (result'^..dirReservations.traversed)
            return $ (M.insert "deployment" name) <$> imds

processReservation :: Reservation
                   -> [InstanceMetaData]
processReservation r = processInstance <$> (r^.rInstances)

processInstance :: Instance
                -> InstanceMetaData
processInstance i =
    let mId   = metaDataFromInstance i
        mTags = metaDataFromTags (i^.i1Tags)
    in mId <> mTags

metaDataFromInstance :: Instance
                     -> InstanceMetaData
metaDataFromInstance i =
    M.fromList [ ("availibility_zone", defaultBlank $ i^.i1Placement.pAvailabilityZone)
               , ("id", i ^.i1InstanceId)
               , ("private_ip", defaultBlank $ i^.i1PrivateIpAddress)
               , ("public_dns", defaultBlank $ i^.i1PublicDnsName)
               , ("public_ip", defaultBlank $ i^.i1PublicIpAddress)
               , ("state", toText $ i^.i1State.isName)
               ]

defaultBlank :: Maybe Text -> Text
defaultBlank = fromMaybe ""

metaDataFromTags :: [Tag]
                 -> InstanceMetaData
metaDataFromTags ts = M.fromList $ convertTag <$> ts
    where convertTag t = ((toLower $ t^.tagKey), (t^.tagValue))
