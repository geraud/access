{-# LANGUAGE OverloadedStrings #-}
module Access (main) where

import Control.Monad    (unless)

import Access.AWS
import Access.CLI
import Access.Config
import Access.Display   (presentResults)
import Access.Execution (executeCommand)
import Access.Process
import Access.Types

main :: IO ()
main = do
    cmd <- getCommandLine
    let predicates = makePredicates $ getPredicates cmd
    unless (canRun cmd) $ error "No predicates specified."
    cfg <- loadConfiguration
    instancesData <- getInstanceDataForAcounts predicates (getAccounts cfg)
    let sortedInstancesData = sortInstanceMetaData (getSortFields cfg) instancesData
    if null sortedInstancesData
    then error "No matches found."
    else runCommand cfg cmd sortedInstancesData
  where
    canRun (Execute []) = False
    canRun _ = True

runCommand :: Configuration -> Command -> [InstanceMetaData] -> IO ()
runCommand cfg (List {}) = presentResults (getFields cfg)
runCommand cfg (Execute {}) = executeCommand (getCommand cfg)
