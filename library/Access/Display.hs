{-# LANGUAGE OverloadedStrings #-}
module Access.Display where

import qualified Data.Map.Strict as M
import           Data.Monoid     ((<>))
import           Data.Text       (Text)
import qualified Data.Text       as T
import qualified Data.Text.IO    as T

import           Access.Types

presentResults :: [Text] -> [InstanceMetaData] -> IO ()
presentResults _ [] = error "No matches to display"
presentResults headers instancesData = do
    let records = getRecords headers instancesData
        sizes = getRecordSizes ([headers] <>recordsWithHeaders)
        recordsWithHeaders = ([headers] <> records)
    mapM_ T.putStrLn $ map (formatRecord sizes) recordsWithHeaders

formatRecord :: [Int] -> [Text] -> Text
formatRecord size entries = T.intercalate "  " $ zipWith formatCell size entries

formatCell :: Int -> Text -> Text
formatCell size contents = T.justifyLeft size ' ' contents

getRecords :: [Text] -> [InstanceMetaData] -> [[Text]]
getRecords keyNames imds = (extractRowsForFields keyNames) <$> imds

extractRowsForFields :: [Text] -> InstanceMetaData -> [Text]
extractRowsForFields fs imd = (\x -> M.findWithDefault "(no data)" x imd) <$> fs

getRecordSizes :: [[Text]] -> [Int]
getRecordSizes [] = []
getRecordSizes records =
    let zero = const 0 <$> (head records)
        in foldr zipMax zero records
  where
    zipMax :: [Text] -> [Int] -> [Int]
    zipMax = zipWith (\txt maxVal -> max maxVal (T.length txt))
