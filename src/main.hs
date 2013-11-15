
{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main (main) where

import Actions (getAct, parseAct)
import System.Environment (getArgs, withArgs)
import System.Console.GetOpt
import System.Exit
import Control.Monad (when)
import System.Console.CmdArgs
import qualified System.Console.CmdArgs as CArg


data Options =
    Parse {
        dir :: String
      , force :: Bool
      }
  | Get {
        name :: FilePath
      , vers :: String
      , getPlatform :: String
      , isList :: Bool
      , isForce :: Bool
      } deriving (Data, Typeable, Show, Eq)

parse :: Options
parse = Parse {
        dir = "." &= help "Directory to parse in"
      , force = False &= help "force downloading"
      }

get :: Options
get = Get {
       name = "name" &= help "thrirdparty name"
     , vers = "version" &= help "thirdparty version"
     , getPlatform = "platform" &= help "thirdparty platform"
     , isList = False &= help "lists all available thirdparties"
     , isForce = False &= help "force downloading"
     }

_TOOL_ABOUT = "Haskell Arifact Storage Management tool \n"
          ++  "3d-party.exe wrapped in Monad :)"
_TOOL_NAME = "harstone.exe"

toolModes :: Mode (CmdArgs Options)
toolModes = cmdArgsMode $ modes [parse, get]
    &= verbosityArgs [explicit, CArg.name "verbose"] []
    &= versionArg [explicit, CArg.name "version", summary "summary"]
    &= help _TOOL_ABOUT
    &= helpArg [explicit, CArg.name "help"]
    &= program _TOOL_NAME

main :: IO()
main = do
  args <- getArgs
  opts <- (if null args then withArgs ["--help"] else id) $ cmdArgsRun toolModes
  optionsHandler opts


optionsHandler :: Options -> IO ()
optionsHandler opts@(Parse dir force ) = do
    putStrLn $ "lets parse in " ++ dir
    parseAct dir force
optionsHandler opts@(Get name vers platform l f) = do
    putStrLn $ "lets get " ++ name ++ vers
    getAct name vers

