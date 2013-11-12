module Types(Artifact(..), Artifacts, Platform) where


data Artifact = Artifact
  {
      artName :: String
    , artVersion :: String
    , artPlatform :: Platform
    , artPath :: String
  } deriving (Show, Eq)

type Artifacts = [Artifact]

data Platform = Windows | MacOS deriving (Show, Read, Eq)

