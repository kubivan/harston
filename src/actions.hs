
module Actions( parseAct
              , getAct
              )where

import Artifact
import Control.Monad {- (liftM, liftM2) -}
import Control.Applicative
import Data.List ((\\), intersect, partition)
import Data.Maybe (catMaybes, isJust)
import qualified Data.Map.Lazy as Map
import System.FilePath.Find {- (fold, always) -}

fromRef :: RefItem -> Aliases -> Maybe Item
fromRef ref =
    Map.lookup $ riName ref

-- boilToItems :: [RefItem] -> Aliases -> ([Item], [RefItem])
boilToItems (als, refs, its) = (res ++ its, notfound)
  where
  --TODO: optimize
    (found, notfound) = partition (isJust.(`fromRef` als)) refs
    res = catMaybes $ map (`fromRef` als) found

parseAct root force = do
   -- downloaded       <- liftM siFromXml $ readFile "downloaded.3dp-manifest"
   downloaded       <- return []
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
         map siItem downloaded \\ foundItems

   existsItems      <- return $ map siItem available `intersect` needToDownload
   print $ "need to download: \n" ++ (show needToDownload)
   return ()


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
