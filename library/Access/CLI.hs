{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -fno-cse #-}
module Access.CLI where

import           Data.Text              (Text)
import           System.Console.CmdArgs

data Command
    = Execute { getPredicates :: [Text] }
    | List    { getPredicates :: [Text] }
    deriving (Show, Data, Typeable)

getCommandLine :: IO Command
getCommandLine = cmdArgsRun command

command :: Mode (CmdArgs Command)
command = cmdArgsMode $ modes [execute &= auto, list]
          &= help "access command for AWS instances"
          &= program "access"
          &= summary "Access v1.0"

execute :: Command
execute = Execute { getPredicates = def &= typ "PREDICATES" &= args }
          &= help "Execute the command in the configuration with the first matching result"

list :: Command
list = List { getPredicates = def &= typ "PREDICATES" &= args }
       &= help "List instances in the defined accounts"

-- explanation from the documentation of cmdargs:
-- {-# OPTIONS_GHC -fno-cse #-}
-- Even using this scheme, sometimes GHC's optimisations may share values who have the same annotation.
-- To disable sharing you may need to specify {-# OPTIONS_GHC -fno-cse #-} in the module you define the flags.
