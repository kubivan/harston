module Tfs (
             Tf
           , createTf
           , runTf
           , tfCreateWS
           , tfRemoveWS
           , tfReadFile
           , tfGet
           , (>>/)
           -- , tfView
           )where

import System.Process
import System.Exit
import System.Directory(getTemporaryDirectory, setCurrentDirectory, getCurrentDirectory)

import Control.Exception(bracket_)
import Control.Monad(liftM)

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

-- (>>/) :: Monad m => m ExitCode -> m ExitCode -> m ExitCode
a >>/ b = do
   rc <- a
   case rc of
    ExitSuccess -> b
    ExitFailure fc -> return rc

tfCreateWS :: Tf -> String -> FilePath -> IO (ExitCode)
tfCreateWS tf name path = do
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
    return rc

tfRemoveWS :: Tf -> String -> IO (ExitCode)
tfRemoveWS tf name =
    runTf tf ["workspace" , name , "/delete" , "/noprompt" , mkCred tf]


tfReadFile :: Tf -> String -> IO (Maybe String)
tfReadFile tf path = do
    let args = ["view", path , "/collection:" ++ tfServer tf , mkCred tf  ]
    (rc, out, err) <- readProcessWithExitCode (tfExe tf) args ""
    print err
    return $ case rc of ExitSuccess -> Just out
                        _ -> Nothing

-- tfGet :: Tf -> FilePath -> FilePath -> Bool -> IO Bool
tfGet tf spath lpath vers rec ws = do
    -- old <- getCurrentDirectory
    -- bracket_ (setCurrentDirectory lpath) (setCurrentDirectory old)
             -- (do
              getCurrentDirectory >>= print
              tfMap tf spath lpath ws
              >>/ runTf tf [ "get", spath, "/all"
                  , if rec then "/recursive" else ""
                  , mkCred tf
                  ]
                  -- )
-- tfMap :: Tf -> FilePath -> FilePath -> IO Bool
tfMap tf spath lpath ws =
    runTf tf [ "workfold", "/map", spath, lpath, "/workspace:" ++ ws
             , "/collection:" ++ tfServer tf, mkCred tf ]

runTf :: Tf -> [String] -> IO ExitCode
runTf tf args  = do
     if tfVerbose tf then print $ "tf.exe" ++ show args else return ()
     (rc, out, err) <- readProcessWithExitCode (tfExe tf) args ""
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
    bracket_ (tfCreateWS tf ws "D:\\3d-party" )
            (tfRemoveWS tf ws)
            (tfGet tf "harstonerepo/Libraries" "D:\\3d-party" "" True ws)
                  -- file <- tfReadFile tf "harstonerepo/available.3dp-manifest"
                  -- >>/ print file

