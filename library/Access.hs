{-# LANGUAGE OverloadedStrings #-}
module Access
    (main) where

import           Control.Concurrent.Async.Lifted (mapConcurrently)
import           Control.Lens
import           Control.Monad                   (when)
import           Data.List                       (sortBy)
import qualified Data.Map.Strict                 as M
import           Data.Monoid                     ((<>))
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import qualified Data.Text.IO                    as T
import           System.Environment
import           System.Exit                     (exitFailure)

import           Access.AWS
import           Access.Config
import           Access.Display                  (presentResults)
import           Access.Types

main :: IO ()
main = do
    (listOnly, predicates) <- processArgs <$> getArgs
    cfg <- loadConfiguration
    instancesData <- concat <$> mapConcurrently (getInstanceData predicates) (cfg ^.accounts)
    let sortedInstancesData = sortInstanceMetaData (cfg ^.sortFields) instancesData
    if null sortedInstancesData
    then error "No matches found."
    else if listOnly then presentResults (cfg ^.fields) sortedInstancesData
                     else executeCommand (cfg ^.command) sortedInstancesData

executeCommand :: Text -> [InstanceMetaData] -> IO ()
executeCommand _ [] = error "No matches found."
executeCommand cmd (imd:_) = do
    T.putStrLn cmd
    print imd

getInstanceData :: [Predicate] -> Account -> IO [InstanceMetaData]
getInstanceData predicates a = do
    instancesData <- loadInstanceData a
    return $ filter (filterResults predicates) instancesData

sortInstanceMetaData :: [Text] -> [InstanceMetaData] -> [InstanceMetaData]
sortInstanceMetaData headers = sortBy (sortByFields headers)

sortByFields :: [Text] -> InstanceMetaData -> InstanceMetaData -> Ordering
sortByFields [] _ _ = EQ
sortByFields (f:fs) imd1 imd2 =
    case (imd1 ^.at f) `compare` (imd2 ^.at f) of
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
