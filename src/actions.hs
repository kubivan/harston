
module Actions( parseAct
              , getAct
              )where

import Artifact
import Control.Monad {- (liftM, liftM2) -}
import Control.Applicative
import Data.List ((\\), intersect)
import Data.Maybe (catMaybes)
import qualified Data.Map.Lazy as Map
import System.FilePath.Find {- (fold, always) -}

fromRef :: RefItem -> Aliases -> Maybe Item
fromRef ref als =
    Map.lookup (riName ref) als


-- returs
boilToItems :: [RefItem] -> Aliases -> ([Item], [String])
boilToItems refs als = (reduced, unfound)
  where
    reduced = catMaybes $ map (`fromRef` als ) refs
    unfound = []

parseAct root force = do
   (als, refs, its) <- parseDir root
   downloaded       <- liftM siFromXml $ readFile "downloaded.3dp-manifest"
   available        <- liftM siFromXml $ readFile "available.3dp-manifest"
   foundRefs        <- return $ catMaybes $ map (`fromRef` als ) refs
   needToDownload   <- return $ its ++ (map siItem downloaded \\ foundRefs)
   existsItems      <- return $ map siItem available `intersect` needToDownload
   -- print available
   print needToDownload
   print $ "can download " ++ (show existsItems)
   -- print als
   -- print refs
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
