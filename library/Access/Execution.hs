{-# LANGUAGE OverloadedStrings #-}
module Access.Execution where

import qualified Data.Map.Strict       as M
import           Data.Text             (Text, pack)
import qualified Data.Text.Lazy        as TL
import           System.Exit           (exitWith)
import           System.Process        (CreateProcess (..), StdStream (..), createProcess, shell,
                                        waitForProcess)
import           Text.Hastache
import           Text.Hastache.Context

import           Access.Types

executeCommand :: Text -> [InstanceMetaData] -> IO ()
executeCommand _ [] = error "No matches found."
executeCommand cmd (imd:_) = do
    command <- render cmd imd
    let cp = (shell command) { std_in = Inherit
                             , std_out = Inherit
                             , std_err= Inherit
                             , env = Nothing
                             }

    putStrLn =<< render "Connecting {{name}} in {{region}} ({{public_dns}})" imd
    (_, _, _, processHandle) <- createProcess cp
    rc <- waitForProcess processHandle
    exitWith rc

render :: Text -> InstanceMetaData -> IO String
render cmd imd = TL.unpack <$> hastacheStr defaultConfig cmd (mkStrContext $ handler imd)
    where handler m name = maybe MuNothing (MuVariable) $ M.lookup (pack name) m
