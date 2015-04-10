{-# LANGUAGE OverloadedStrings #-}
module Access.Process where

import           Control.Concurrent.Async.Lifted (mapConcurrently)
import           Data.List                       (sortBy)
import qualified Data.Map.Strict                 as M
import           Data.Monoid                     ((<>))
import           Data.Text                       (Text)
import qualified Data.Text                       as T

import           Access.AWS
import           Access.Types

getInstanceDataForAcounts :: [Predicate] -> [Account] -> IO [InstanceMetaData]
getInstanceDataForAcounts predicates accounts = concat <$> mapConcurrently (getInstanceData predicates) accounts

getInstanceData :: [Predicate] -> Account -> IO [InstanceMetaData]
getInstanceData predicates a = do
    instancesData <- loadInstancesMetaData a
    return $ filter (filterResults predicates) instancesData

sortInstanceMetaData :: [Text] -> [InstanceMetaData] -> [InstanceMetaData]
sortInstanceMetaData headers = sortBy (sortByFields headers)

sortByFields :: [Text] -> InstanceMetaData -> InstanceMetaData -> Ordering
sortByFields [] _ _ = EQ
sortByFields (f:fs) imd1 imd2 =
    case (imd1 M.! f) `compare` (imd2 M.! f) of
        EQ -> sortByFields fs imd1 imd2
        res -> res

makePredicates :: [Text] -> [Predicate]
makePredicates args = [KeyValueMatcher "state" "running"] <> (ValueMatcher <$> args)

filterResults :: [Predicate] -> InstanceMetaData -> Bool
filterResults predicates imd = all (match imd) predicates

match :: InstanceMetaData -> Predicate -> Bool
match m (KeyValueMatcher k v) = maybe False (v `T.isPrefixOf`) (M.lookup k m)
match m (ValueMatcher v) = not . M.null $ M.filter (v `T.isPrefixOf`) m
