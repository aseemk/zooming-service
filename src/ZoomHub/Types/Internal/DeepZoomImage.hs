{-# LANGUAGE DeriveGeneric #-}

module ZoomHub.Types.Internal.DeepZoomImage
  ( DeepZoomImage(DeepZoomImage)
  , dziWidth
  , dziHeight
  , dziTileSize
  , dziTileOverlap
  , dziTileFormat
  ) where

import           Data.Aeson        (FromJSON, ToJSON, genericParseJSON,
                                    genericToJSON, parseJSON, toJSON)
import           Data.Aeson.Casing (aesonPrefix, camelCase)
import           GHC.Generics      (Generic)


data DeepZoomImage = DeepZoomImage
  { dziWidth       :: Integer
  , dziHeight      :: Integer
  , dziTileSize    :: Integer
  , dziTileOverlap :: Integer
  , dziTileFormat  :: String
  } deriving (Eq, Show, Generic)

instance ToJSON DeepZoomImage where
   toJSON = genericToJSON $ aesonPrefix camelCase
instance FromJSON DeepZoomImage where
   parseJSON = genericParseJSON $ aesonPrefix camelCase
