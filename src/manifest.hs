module Manifest (fromXml, toXml ) where

import Types

import Text.XML.HaXml
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Posn
import qualified Text.XML.HaXml.Pretty as P
import Text.XML.HaXml.Html.Generate (showattr)
import Control.Applicative ((<$>), (<*>), ZipList(..))
import Control.Monad (liftM, liftM2)

-- Get artifacts info from manifest file
fromXml :: String -> Artifacts
fromXml content =
      fromXml' doc
    where
      parseResult = xmlParse "error.log" content
      doc = getContent parseResult

      getContent :: Document Posn -> Content Posn
      getContent (Document _ _ e _) = CElem e noPos

fromXml' :: Content Posn -> Artifacts
fromXml' doc =
    map artifactItem $ artifact doc
    where
      -- type CFilter = Content i -> Content [i]
      artifact :: CFilter Posn
      artifact = tag "_3dp-manifest" /> tag "third-party"
      artifactItem :: Content Posn -> Artifact
      artifactItem item = Artifact name vers platform path
        where
            name = contentToStr . showattr "Name" $ item
            vers = contentToStr . showattr "Version" $ item
            platform = read . contentToStr . showattr "Platforms" $ item
            path = contentToStr . (keep /> tag "path" /> txt) $ item

toXml :: Artifacts -> [Content Posn]
toXml arts = manifest fakeContent
    where
      manifest = mkElem "_3dp_manifest" makeArtifacts
      fakeContent = CElem (Elem (N "fake") [] []) noPos
      makeArtifacts :: [CFilter Posn]
      makeArtifacts = makeArtifact <$> arts

makeArtifact :: Artifact -> CFilter Posn
makeArtifact art =
    mkElemAttr  "thrird-party" attrs content
    where
      attrs :: [ (String, CFilter Posn)]
      attrs =  [ ("Name",      literal $ artName art)
               , ("Version",   literal $ artVersion art)
               , ("Platforms", literal $ show . artPlatform $ art)
               ]

      content = [mkElem "path" [literal $ artPath art]]

contentToStr :: [Content Posn] -> String
contentToStr =
    concatMap procContent
    where procContent x =
              verbatim $ keep /> txt $ CElem (unesc (fakeElem x)) noPos
          fakeElem :: Content Posn -> Element Posn
          fakeElem x = Elem (N "fake") [] [x]

          unesc :: Element Posn -> Element Posn
          unesc = xmlUnEscape stdXmlEscaper
testParse fname = do
  xml <- readFile fname
  return $ fromXml xml

-- testGen :: IO [Content Posn]
testGen =
      readFile "short.xml"
  >>= return . fromXml
  >>= writeFile "out.xml". show . P.content. head. toXml
