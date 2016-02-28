{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Types.DeepZoomImage
  ( DeepZoomImage(DeepZoomImage)
  , DeepZoomImageURI(DeepZoomImageURI)
  , dziHeight
  , dziTileFormat
  , dziTileOverlap
  , dziTileSize
  , dziUrl
  , dziWidth
  , fromInternal
  , mkDeepZoomImage
  ) where

import           Data.Aeson                           (ToJSON, Value (String),
                                                       genericToJSON, toJSON)
import           Data.Aeson.Casing                    (aesonPrefix, camelCase)
import           Data.Maybe                           (fromJust)
import qualified Data.Text                            as T
import           GHC.Generics                         (Generic)
import           Network.URI                          (URI,
                                                       parseRelativeReference,
                                                       relativeTo)

import           ZoomHub.Types.ContentBaseURI         (ContentBaseURI,
                                                       unContentBaseURI)
import           ZoomHub.Types.Internal.ContentId     (ContentId, unId)
import qualified ZoomHub.Types.Internal.DeepZoomImage as Internal


data DeepZoomImage = DeepZoomImage
  { dziUrl         :: DeepZoomImageURI
  , dziWidth       :: Integer
  , dziHeight      :: Integer
  , dziTileSize    :: Integer
  , dziTileOverlap :: Integer
  , dziTileFormat  :: String
  } deriving (Eq, Show, Generic)

fromInternal :: ContentBaseURI ->
                ContentId ->
                Internal.DeepZoomImage ->
                DeepZoomImage
fromInternal baseURI cId dzi = DeepZoomImage
  { dziUrl = DeepZoomImageURI (dziPath `relativeTo` unContentBaseURI baseURI)
  , dziWidth = Internal.dziWidth dzi
  , dziHeight = Internal.dziHeight dzi
  , dziTileSize = Internal.dziTileSize dzi
  , dziTileOverlap = Internal.dziTileOverlap dzi
  , dziTileFormat = Internal.dziTileFormat dzi
  }
  where
    dziPath = fromJust . parseRelativeReference $ "/dzis/" ++ unId cId ++ ".dzi"

mkDeepZoomImage :: DeepZoomImageURI ->
                   Integer ->
                   Integer ->
                   Integer ->
                   Integer ->
                   String ->
                   DeepZoomImage
mkDeepZoomImage uri width height tileSize tileOverlap tileFormat = DeepZoomImage
  { dziUrl = uri
  , dziWidth = width
  , dziHeight = height
  , dziTileSize = tileSize
  , dziTileOverlap = tileOverlap
  , dziTileFormat = tileFormat
  }

-- JSON
instance ToJSON DeepZoomImage where
   toJSON = genericToJSON $ aesonPrefix camelCase

-- Types
newtype DeepZoomImageURI = DeepZoomImageURI { unDeepZoomImageURI :: URI }
  deriving (Eq)

instance Show DeepZoomImageURI where
  show = show . unDeepZoomImageURI

instance ToJSON DeepZoomImageURI where
  toJSON = String . T.pack . show . unDeepZoomImageURI
