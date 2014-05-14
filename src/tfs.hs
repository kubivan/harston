module Tfs (
             Tf
           , createTf
           , runTf
           , tfCreateWS
           , tfRemoveWS
           , tfReadFile
           , tfGet
           , runMaybeT
           -- , tfView
           )where

import System.Process
import System.Exit
import System.Directory(getTemporaryDirectory, setCurrentDirectory, getCurrentDirectory)

import Control.Exception(bracket_)
import Control.Monad(liftM, guard)
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans(lift)

data Tf =
  Tf {
    tfExe     :: String
  , tfServer  :: String
  , tfWs      :: String
  , tfCreds   :: Maybe (String, String)
  , tfVerbose :: Bool
  }


createTf :: String -> String ->String -> Maybe (String, String) -> Tf
createTf toolpath spath ws creds =
      Tf "C:\\Program Files (x86)\\Microsoft Visual Studio 11.0\\Common7\\IDE\\tf.exe"
             spath ws creds True

tfCreateWS :: Tf -> String -> FilePath -> MaybeT IO ()
tfCreateWS tf name path = MaybeT $ do
    let args = [ "workspace"
               ,  name
               , "/new"
               , "/noprompt"
               , "/collection:" ++ tfServer tf
               , mkCred tf
               , "/comment: temporary workspace"
               ]
    old <- getCurrentDirectory
    setCurrentDirectory path
    rc <- runTf tf args
    setCurrentDirectory old
    return $ guard (rc /= ExitSuccess)

tfRemoveWS :: Tf -> String -> MaybeT IO ()
tfRemoveWS tf name = MaybeT $
    runTf tf ["workspace" , name , "/delete" , "/noprompt" , mkCred tf]
    >>= \rc -> return $ guard(rc /= ExitSuccess)


tfReadFile :: Tf -> String -> IO (Maybe String)
tfReadFile tf path = do
    let args = ["view", path , "/collection:" ++ tfServer tf , mkCred tf  ]
    (rc, out, err) <- readProcessWithExitCode (tfExe tf) args ""
    print err
    return $ case rc of ExitSuccess -> Just out
                        _ -> Nothing

tfGet :: Tf -> FilePath -> FilePath ->String -> Bool -> String -> MaybeT IO ()
tfGet tf spath lpath vers rec ws = do
              cd <- lift getCurrentDirectory
              lift $ print cd
              tfMap tf spath lpath ws
              grc <- lift (runTf tf [ "get", spath, "/all"
                          , if rec then "/recursive" else ""
                          , mkCred tf
                          ])

              lift $ print grc
              guard(grc == ExitSuccess)

tfMap :: Tf -> FilePath -> FilePath ->String -> MaybeT IO ()
tfMap tf spath lpath ws = do
      lift $ print "mapping..."
      rc <- lift (runTf tf [ "workfold", "/map", spath, lpath, "/workspace:" ++ ws
                           , "/collection:" ++ tfServer tf, mkCred tf ])
      lift $ print rc
      guard (rc == ExitSuccess)
      lift $ print "mapping created..."

runTf :: Tf -> [String] -> IO ExitCode
runTf tf args  = do
     if tfVerbose tf then print $ "tf.exe" ++ show args else return ()
     -- (rc, out, err) <- readProcessWithExitCode (tfExe tf) args ""
     (rc, out, err) <- return (ExitSuccess, "output", "errors")
     if tfVerbose tf then
         print out >> print err
     else
        return ()
     return rc


mkCred tf = case tfCreds tf of
       Just (name, pass) -> "/login:" ++ name ++"," ++ pass
       _                 -> ""

-- testTf pass = initTf " "  "https://tfs.codeplex.com:443/tfs/TFS24/"  $ Just ("snd\\ikubarev_cp", pass)

testTf pass = createTf " "  "https://tfs.codeplex.com:443/tfs/TFS24/" "tempws" Nothing
testTfs pass = do
    let tf  = testTf pass
        ws  = "tempws"
    bracket_ (runMaybeT $ tfCreateWS tf ws "D:\\3d-party" )
             (runMaybeT $ tfRemoveWS tf ws)
             (runMaybeT $ tfGet tf "harstonerepo/Libraries" "D:\\3d-party" "" True ws)
                  -- file <- tfReadFile tf "harstonerepo/available.3dp-manifest"
                  -- >>/ print file

