
module Artifact
             ( Item (itName, itVersion, itPlatform)
             , ServerItem (siPath, siItem)
             , RefItem (riName)
             , Artifacts
             , Aliases
             , itFromXml
             , siFromXml
             , riFromXml
             , aiFromXml
             )
            where

import Text.XML.HaXml
import Text.XML.HaXml.Parse
import Text.XML.HaXml.XmlContent(fWriteXml )
import Text.XML.HaXml.Posn
import qualified Text.XML.HaXml.Pretty as P
import Text.XML.HaXml.Html.Generate (showattr)
import Control.Applicative ((<$>) )
import qualified Data.Map.Lazy as Map

data Item =
    Item
    {
      itName :: String
    , itVersion :: String
    , itPlatform :: Platform
    } deriving (Show, Eq)

data ServerItem = ServerItem
    {
      siPath :: String
    , siItem :: Item
    }deriving (Show, Eq)

type Alias = (String, Item)

data RefItem = RefItem
    {
      riName :: String
    } deriving (Show, Eq)

type XmlString = String

type Artifacts = ([Item], [RefItem], Aliases, [ServerItem])
type Aliases = Map.Map String Item

data Platform = Windows | MacOS deriving (Show, Read, Eq)

itFromXml :: XmlString -> [Item]
itFromXml xml =
    map plainItem $ fman /> fplain $ doc
    where
      doc = contentFromXml xml

siFromXml :: XmlString -> [ServerItem]
siFromXml xml =
    map serverItem $ fman /> fserver $ doc
    where
      doc = contentFromXml xml

aiFromXml :: XmlString -> Aliases
aiFromXml xml =
    Map.fromList al
    where
      al = map aliasItem $ fman /> tag "alias" $ doc
      doc = contentFromXml xml

riFromXml :: XmlString -> [RefItem]
riFromXml xml =
    map refItem $ fman /> fref $ doc
    where
      doc = contentFromXml xml


-- auxilary filters
-- TODO: optimize
fman    = tag "_3dp-manifest"
fitem   = tag "third-party" `o` attr "Name" `o` attr "Platforms" `o` attr "Version"
fplain  = fitem `without` (fitem /> tag "path")
fserver = fitem `with` (fitem /> tag "path")
fref    = (tag "third-party" `o` attr "Name") `without` (tag "third-party" `o` attr "Platforms" `o` attr "Version")

-- type CFilter = Content i -> Content [i]
plainItem :: Content Posn -> Item
plainItem item = Item name vers platform
  where
      name = contentToStr.showattr "Name" $ item
      vers = contentToStr.showattr "Version" $ item
      platform = read.contentToStr.showattr "Platforms" $ item

serverItem :: Content Posn -> ServerItem
serverItem citem = ServerItem path plain
  where
    path =  contentToStr.(keep /> tag "path" /> txt) $ citem
    plain = plainItem citem

aliasItem :: Content Posn -> Alias
aliasItem citem = (name, it)
  where
    name = contentToStr.showattr "Name" $ citem
    it = plainItem $ head (keep /> tag "third-party" $ citem)

refItem :: Content Posn -> RefItem
refItem citem = RefItem $ contentToStr.showattr "Name" $ citem

--hepres
contentFromXml :: XmlString -> Content Posn
contentFromXml content = doc
    where
      parseResult = xmlParse "error.log" content
      doc = getContent parseResult

      getContent :: Document Posn -> Content Posn
      getContent (Document _ _ e _) = CElem e noPos

contentToStr :: [Content Posn] -> String
contentToStr =
    concatMap procContent
    where procContent x =
              verbatim $ keep /> txt $ CElem (unesc (fakeElem x)) noPos
          fakeElem :: Content Posn -> Element Posn
          fakeElem x = Elem (N "fake") [] [x]

          unesc :: Element Posn -> Element Posn
          unesc = xmlUnEscape stdXmlEscaper


makeDocument :: Content Posn -> Document Posn
makeDocument (CElem el _) = Document prolog [] el []
    where
      xmldecl = Just $ XMLDecl "1.0" (Just $ EncodingDecl "UTF-8") Nothing
      prolog = Prolog xmldecl [] Nothing []


getAll :: XmlString -> ([Item], [ServerItem], Aliases, [RefItem])
getAll xml = (a, b, c, d)
  where
    a = itFromXml xml
    b = siFromXml xml
    c = aiFromXml xml
    d = riFromXml xml

testParse fname = do
  xml <- readFile fname
  print.show.length $ xml
  return $ getAll xml


-- siToXml :: [ServerItem] ->
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
