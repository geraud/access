{-# LANGUAGE OverloadedStrings #-}
module Access.Display where

import           Data.Text                       (Text)
import qualified Data.Text                       as T
import qualified Data.Text.IO                    as T

presentResults :: [Int] -> [[Text]] -> IO ()
presentResults sizes records = mapM_ T.putStrLn $ map (formatRecord sizes) records

formatRecord :: [Int] -> [Text] -> Text
formatRecord size entries = T.intercalate " " $ zipWith formatCell size entries

formatCell :: Int -> Text -> Text
formatCell size contents = T.justifyLeft size ' ' contents
