
module Actions( parseAct
              , getAct
              )where

import Artifact
import Tfs
import Control.Monad {- (liftM, liftM2) -}
import Control.Applicative
import Data.List ((\\), intersect, partition)
import Data.Maybe (catMaybes, isJust)
import qualified Data.Map.Lazy as Map
import System.FilePath.Find (find, always, extension, (==?))
import System.Directory(setCurrentDirectory, getCurrentDirectory, renameDirectory)
import Control.Exception(bracket_)
import qualified Data.List as List (find)

fromRef :: RefItem -> Aliases -> Maybe Item
fromRef ref =
    Map.lookup $ riName ref

--takes item references and aliases and resolves to items
--along with unresolved references
resolveRefs:: Aliases -> [RefItem] -> ([Item], [RefItem])
resolveRefs als refs = (res , notfound)
  where
  --TODO: optimize
    (found, notfound) = partition (isJust.(`fromRef` als)) refs
    res = catMaybes $ map (`fromRef` als) found

--takes list of required items, list of already downloaded items and returns list
--of items we need to download along with list of "bad" items
toDownload :: [Item] -> [ServerItem] -> [ServerItem] -> ([ServerItem],[Item])
toDownload required downloaded available = (foundIn available, [] )
  where
    needed = required \\ map siItem downloaded
    foundIn = filter (`existIn` needed)
    existIn si = isJust.( List.find (\it -> it == siItem si))

-- takes list of xmlfiles and returns list of required items
-- along with unresolved references
getRequiredItems xmls = (reqs, unresolved)
  where
    reqs = resolved ++ (concat $ map itFromXml xmls)
    (resolved, unresolved) = resolveRefs als (concat $ map riFromXml xmls)
    als  = foldl Map.union Map.empty (map aiFromXml xmls)

--finds all items we need to download
collectItems root force= do
  downloaded       <- liftM siFromXml $ readFile "downloaded.3dp-manifest"
  available        <- liftM siFromXml $ readFile "available.3dp-manifest"
  (required, unresolved) <- liftM getRequiredItems $ parseDir root
  unless (null unresolved) $ print $ "Warning:unresolved references : \n" ++ show unresolved
  let (needed, notfound) = toDownload required downloaded available
  unless (null notfound) $
    print $ "Warning: the following items not found in available.manifest: \n"
    ++ show notfound

  return needed

parseAct root force = do
  items <- collectItems root force
  print $ "items \n" ++ show items
  let ws = "tempws"
      tf = createTf " " "https://tfs.codeplex.com:443/tfs/TFS24/" ws Nothing
      rdir = "D:\\3d-party"

  old <- getCurrentDirectory
  bracket_ (setCurrentDirectory rdir >> tfCreateWS tf ws rdir)
           (setCurrentDirectory old  >> tfRemoveWS tf ws)
           (sequence $ map (downloadSitem tf ws) items)


downloadSitem tf ws si = do
    let name  = siName si ++ "_" ++ siVersion si
        tname = "~" ++ name
    print $ "Downloading " ++ name
    tfGet tf (siPath si) tname "" True ws
    renameDirectory tname name

parseDir root = find always (extension ==? ".3dp-manifest" ) root >>= mapM readFile

getAct :: String -> String -> IO ()
getAct = undefined
