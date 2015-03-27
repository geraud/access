{-# LANGUAGE OverloadedStrings #-}
module Access
    (main) where

import           Control.Concurrent.Async.Lifted (mapConcurrently)
import           Control.Lens
import qualified Data.Map.Strict                 as M
import           Data.Monoid                     ((<>))
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Prelude                         hiding (error)
import           System.Environment

import           Access.AWS
import           Access.Config
import           Access.Types
import           Access.Display

{-
ssh -t -q -A -p 10022 -o Compression=yes access.%{region}.ops.cardspring.net
"exec ssh -q -p 10022 -A -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no -p 10022 %{hostname}"
-}

main :: IO ()
main = do
    (_listOnly, predicates) <- processArgs <$> getArgs
    cfg <- loadConfiguration
    instancesData <- concat <$> mapConcurrently (getInstanceData predicates) (cfg ^.accounts)
    let headers = (cfg ^.fields)
        records = getRecords headers instancesData
        recordsWithHeaders = [headers] ++ records
        sizes = getRecordSizes recordsWithHeaders
    presentResults sizes recordsWithHeaders

getInstanceData :: [Predicate] -> Account -> IO [InstanceMetaData]
getInstanceData predicates a = do
    instancesData <- loadInstanceData a
    return $ filter (filterResults predicates) instancesData

getRecordSizes :: [[Text]] -> [Int]
getRecordSizes [] = []
getRecordSizes records =
    let zero = map (const 0) (head records)
        in foldr zipMax zero records
  where
    zipMax :: [Text] -> [Int] -> [Int]
    zipMax = zipWith (\txt maxVal -> max maxVal (T.length txt))

getRecords :: [Text] -> [InstanceMetaData] -> [[Text]]
getRecords keyNames imds = map (extractRowsForFields keyNames) imds

extractRowsForFields :: [Text] -> InstanceMetaData -> [Text]
extractRowsForFields fs imd = map (\x -> M.findWithDefault "(no data)" x imd) fs

processArgs :: [String] -> (Bool, [Predicate])
processArgs ("list":args) = (True, makePredicates args)
processArgs args = (False, makePredicates args)

makePredicates :: [String] -> [Predicate]
makePredicates args = [KeyValueMatcher "state" "running"] <> (ValueMatcher . T.pack <$> args)

filterResults :: [Predicate] -> InstanceMetaData -> Bool
filterResults predicates imd = all (match imd) predicates

match :: InstanceMetaData -> Predicate -> Bool
match m (KeyValueMatcher k v) = maybe False (v `T.isPrefixOf`) (M.lookup k m)
match m (ValueMatcher v) = not . M.null $ M.filter (v `T.isPrefixOf`) m
