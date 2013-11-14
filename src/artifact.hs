
module Artifact
            -- ( Artifact(..)
            -- , Artifacts
            -- , Aliases
            -- , Platform
            -- , fromXml
            -- , toXml
            -- , aliasFromXml
            -- )
            where

import Text.XML.HaXml
import Text.XML.HaXml.Parse
import Text.XML.HaXml.XmlContent(fWriteXml )
import Text.XML.HaXml.Posn
import qualified Text.XML.HaXml.Pretty as P
import Text.XML.HaXml.Html.Generate (showattr)
import Control.Applicative ((<$>) )

data Item =
    Item
    {
      itemName :: String
    , itemVersion :: String
    , itemPlatform :: Platform
    } deriving (Show, Eq)

data Artifact =
    Plain Item
  | Server
    {
      sPath :: String
    , sItem :: Item
    }
  | Alias
    {
      alName :: String
    , alItem :: Item
    }
  | Ref
    {
      refName :: String
    } deriving (Show, Eq)

-- getItem is for internal use only so error is ok
getItem :: Artifact -> Item
getItem (Server _ it )= it
getItem (Alias _ it) = it
getItem (Plain it) = it
getItem _ = error "invalid item"

isAlias :: Artifact -> Bool
isAlias (Alias _ _) = True
isAlias _ = False

isRef (Ref _) = True
isRef _ = False

isPlain (Plain _) = True
isPlain _ = False

data Platform = Windows | MacOS deriving (Show, Read, Eq)

fromXml :: String -> [Artifact]
fromXml content =
      fromXml' doc
    where
      parseResult = xmlParse "error.log" content
      doc = getContent parseResult

      getContent :: Document Posn -> Content Posn
      getContent (Document _ _ e _) = CElem e noPos

fromXml' :: Content Posn -> [Artifact]
fromXml' doc =
        (map plainItem $ fman /> fplain $ doc)
     ++ (map aliasItem $ fman /> tag "alias" $ doc)
     ++ (map serverItem $ fman /> fserver $ doc)
     ++ (map refItem $ fman /> fref $ doc)
     where
        fman    = tag "_3dp-manifest"
        fitem   = tag "third-party" `o` attr "Name" `o` attr "Platforms" `o` attr "Version"
        fplain  = fitem `without` (fitem /> tag "path")
        fserver = fitem `with` (fitem /> tag "path")
        fref    = (tag "third-party" `o` attr "Name") `without` (tag "third-party" `o` attr "Platforms" `o` attr "Version")

-- type CFilter = Content i -> Content [i]
plainItem :: Content Posn -> Artifact
plainItem item = Plain $ Item name vers platform
  where
      name = contentToStr.showattr "Name" $ item
      vers = contentToStr.showattr "Version" $ item
      platform = read.contentToStr.showattr "Platforms" $ item

serverItem :: Content Posn -> Artifact
serverItem citem = Server path $ getItem plain
  where
    path =  contentToStr.(keep /> tag "path" /> txt) $ citem
    plain = plainItem citem

aliasItem :: Content Posn -> Artifact
aliasItem content = Alias name $ getItem art
  where
    name = contentToStr.showattr "Name" $ content
    art = plainItem $ head (keep /> tag "third-party" $ content)

refItem :: Content Posn -> Artifact
refItem citem = Ref $ contentToStr.showattr "Name" $ citem

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
  print.show.length $ xml
  return $ fromXml xml


-- toXml :: Artifacts -> [Content Posn]
-- toXml arts = manifest fakeContent
--     where
--       manifest = mkElem "_3dp-manifest" makeArtifacts
--       fakeContent = CElem (Elem (N "fake") [] []) noPos
--       makeArtifacts :: [CFilter Posn]
--       makeArtifacts = makeArtifact <$> arts
--
-- makeArtifact :: Artifact -> CFilter Posn
-- makeArtifact art =
--     mkElemAttr  "third-party" attrs content
--
--     where
--       name = artName art
--       vers = artVersion art
--       plat = artPlatform art
--       path = artPath art
--
--       attrs :: [ (String, CFilter Posn)]
--       attrs = [("Name", literal name)]
--             -- ++ maybe [] ((:[]).(,) "Version".literal) vers
--             ++ maybe [] (\v->[("Version", literal v)]) vers
--             ++ maybe [] (\p->[("Platforms", literal $ show p)]) plat
--
--       content = maybe [] (\p-> [mkElem "path" [literal p] ]) path
--
-- makeDocument :: Content Posn -> Document Posn
-- makeDocument (CElem el _) = Document prolog [] el []
--     where
--       xmldecl = Just $ XMLDecl "1.0" (Just $ EncodingDecl "UTF-8") Nothing
--       prolog = Prolog xmldecl [] Nothing []
--
--
-- testParseAlias fname = do
--   xml <- readFile fname
--   print.show.length $ xml
--   return $ aliasFromXml xml
-- -- testGen :: IO [Content Posn]
-- testGen fname =
--       readFile fname
--   >>= return . fromXml
--   >>= return . show .P.document. makeDocument .head. toXml
--   >>= \res -> writeFile "out.3dp-manifest" res
--   -- >> fWriteXml "out2.xml" res
--   >> print res
