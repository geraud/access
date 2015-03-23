{-# LANGUAGE OverloadedStrings #-}
module Access (main) where

import           Control.Applicative             ((<$>))
import           Control.Concurrent.Async.Lifted (mapConcurrently)
import           Control.Monad                   (forM_)
import qualified Data.Map.Strict                 as M
import           Data.Text                       (Text, isPrefixOf)
import           Prelude                         hiding (error)

import           Access.AWS
import           Access.Config

main :: IO ()
main = do
    putStrLn "Starting access"
    envs <- loadConfiguration
    instancesData <- concat <$> mapConcurrently loadInstanceData envs
    let filteredData = filter (filterResults matchers) instancesData
    forM_ filteredData print
    return ()
  where
    matchers = [KeyValueMatcher "state" "running", ValueMatcher "build", ValueMatcher "54.221.62.151"]

data Predicate
    = KeyValueMatcher Text Text
    | ValueMatcher Text

match :: InstanceMetaData -> Predicate -> Bool
match m (KeyValueMatcher k v) = maybe False (v `isPrefixOf`) (M.lookup k m)
match m (ValueMatcher v) = not . M.null $ M.filter (v `isPrefixOf`) m

filterResults :: [Predicate] -> InstanceMetaData -> Bool
filterResults predicates imd = any (match imd) predicates
