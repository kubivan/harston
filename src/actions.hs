

module Actions( parseAct
              , getAct
              )where

import Artifact
import Manifest
import Control.Monad {- (liftM, liftM2) -}
import Control.Applicative
import Data.List ((\\))


splitAlises2 :: FilePath -> Bool -> IO ()
splitAlises2 root _ =
      parseDir root
  >>= \dirArts -> return (filter isAlias dirArts)
  >>= \aliases -> print aliases
  >>  return (dirArts \\ aliases)
  >>= \reqs -> print reqs

-- parseAct3 :: FilePath -> Bool -> IO ()
splitArtifacts :: [Artifact] -> ([Artifact],[Artifact],[Artifact])
splitArtifacts arts = (reqs, refs, aliases)
  where
    aliases = filter isAlias arts
    reqs = filter isPlain arts
    refs = filter isRef arts

parseAct root force = do
   (reqs,refs,aliases) <- liftM splitArtifacts $ parseDir root
   downloaded      <- parseFile "downloaded.3dp-manifest"
   available       <- parseFile "available.3dp-manifest"
   return ()
   print reqs
   print aliases
   print refs


getAct :: String -> String -> IO ()
getAct = undefined

-- parseAct :: FilePath -> Bool -> IO ()
-- -- parseAct root force =
-- --   let
-- --       -- serverArts = parseFile "available.3dp-manifest"
-- --       -- availArts = parseFile "downloaded.3dp-manifest"
-- --       dirArts = parseDir root -- all items found
-- --       aliases = liftM (filter isAlias) dirArts
-- --       reqs    = liftM2 (\\) dirArts aliases
-- --
-- --   in
-- --       return ()
-- --   >>  aliases
-- --   >>= print
-- --   >>  print "______________________"
-- --   >>  reqs
-- --   >>= print
-- --   -- >>  print <$> aliases
-- --   -- >>  root ++ " dependencies"
-- --   -- >>  print
--
