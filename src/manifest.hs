

module Manifest  where

import Artifact
import System.FilePath.Find {- (fold, always) -}
import Control.Monad
import Data.List( (\\) ,deleteFirstsBy)

-- Get all artifact items from file
parseFile:: FilePath -> IO [Artifact]
parseFile fname = do
  print $ "Processing " ++ fname ++ "..."
  xml <- readFile fname
  res <- return.fromXml $ xml
  print $ "Artifact items found: " ++ (show.length $ res)
  return $ res

parseDir :: FilePath -> IO [Artifact]
parseDir root =
      find always (extension ==? ".3dp-manifest" ) root
   >>= mapM parseFile
   >>= return.concat


-- -- getRequiredArtifacts :: FilePath -> Artifacts
-- getRequiredArtifacts root =
--       let
--         reqArts = getArtifacts root
--         aliases = getAliases root
--         downloadedArts = parseFile "downloaded.3dp-manifest"
--         availArts = parseFile "available.3dp-manifest"
--
--         pred :: Artifact -> Artifact -> Bool
--         pred a b = (artName a == artName b)
--                 && (artVersion a == artVersion b)
--                 && (artPlatform a == artPlatform b)
--
--       in
--         liftM2 (deleteFirstsBy pred) reqArts downloadedArts
--         >>= \needed -> print some
--         >>  liftM (deleteFirstsBy pred some) availArts
--         -- now look into aliases
--         -- >>= \notFound-> return.map (find (\art -> artName art == )
