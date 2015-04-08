{-# LANGUAGE OverloadedStrings #-}
module Access
    (main) where

import           Control.Concurrent.Async.Lifted (mapConcurrently)
import           Control.Monad                   (when)
import           Data.List                       (sortBy)
import qualified Data.Map.Strict                 as M
import           Data.Monoid                     ((<>))
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           System.Environment              (getArgs)

import           Access.AWS
import           Access.Config
import           Access.Display                  (presentResults)
import           Access.Execution                (executeCommand)
import           Access.Types

main :: IO ()
main = do
    (listOnly, predicates) <- processArgs <$> getArgs
    when (not listOnly && length predicates == 1) $ error "No predicates specified."
    cfg <- loadConfiguration
    instancesData <- concat <$> mapConcurrently (getInstanceData predicates) (getAccounts cfg)
    let sortedInstancesData = sortInstanceMetaData (getSortFields cfg) instancesData
    if null sortedInstancesData
    then error "No matches found."
    else if listOnly then presentResults (getFields cfg) sortedInstancesData
                     else executeCommand (getCommand cfg) sortedInstancesData

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
