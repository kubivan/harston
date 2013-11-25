
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
import System.FilePath.Find {- (fold, always) -}
import System.Directory
import Control.Exception(bracket_)
import qualified Data.List(find)

fromRef :: RefItem -> Aliases -> Maybe Item
fromRef ref =
    Map.lookup $ riName ref

-- boilToItems :: [RefItem] -> Aliases -> ([Item], [RefItem])
boilToItems (als, refs, its) = (res ++ its, notfound)
  where
  --TODO: optimize
    (found, notfound) = partition (isJust.(`fromRef` als)) refs
    res = catMaybes $ map (`fromRef` als) found


-- m :: [Item] -> [ServerItem] -> [ServerItem]
m its sits = catMaybes $ zipWith (\it sit -> if it == siItem sit then Just sit else Nothing) its sits

m2 sits its =
  filter (\si-> isJust (Data.List.find (\it -> it == siItem si) its )) sits

collectSitems root force = do
   downloaded       <- liftM siFromXml $ readFile "downloaded.3dp-manifest"
   available        <- liftM siFromXml $ readFile "available.3dp-manifest"
   (foundItems, nfRefs) <- liftM boilToItems $ parseDir root
   if (not.null $ nfRefs) then
      print $ "Warning: the following items do not exist: \n"
            ++ show nfRefs
      else return ()

   print foundItems
   needToDownload   <- return $ if force then
         foundItems
      else
         foundItems \\ map siItem downloaded

   existsItems      <- return $ map siItem available `intersect` needToDownload
   -- print $ "need to download: \n" ++ (show needToDownload)
   -- print $ "available"
   return $ m2 available needToDownload

parseAct root force = do
  items <- collectSitems root force
  print $ "items \n" ++ show items
  let ws = "tempws"
      tf = createTf " " "https://tfs.codeplex.com:443/tfs/TFS24/" ws Nothing
      rdir = "D:\\3d-party"
  setCurrentDirectory rdir
  bracket_ (tfCreateWS tf ws rdir)
           (tfRemoveWS tf ws)
           (sequence( map (downloadSitem tf ws) items))


downloadSitem tf ws si = do
    let name  = siName si ++ "_" ++ siVersion si
        tname = "~" ++ name
    print $ "Downloading " ++ name
    tfGet tf (siPath si) tname "" True ws
    renameDirectory tname name


-- look for aliase references and items in directory
-- there is no need to search for ServerItems since they stored
-- in available and downloaded manifest only
-- parseDir :: FilePath -> IO ([Alias],[RefItem],[Item])
parseDir root = do --(als, refs, its)
       find always (extension ==? ".3dp-manifest" ) root
   >>= mapM readFile
   >>= \xmls -> return (map aiFromXml xmls)
   --TODO: warn against duplicates
   >>= \als  -> return (map riFromXml xmls)
   >>= \refs -> return (map itFromXml xmls)
   >>= \its  -> return (foldl Map.union Map.empty als, concat refs, concat its)

getAct :: String -> String -> IO ()
getAct = undefined
