{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
-- |

module Hrt.Scene where

import Data.Aeson (FromJSON, parseJSON, (.:), withObject, eitherDecodeFileStrict)
import Data.Colour
import GHC.Generics
import Linear
import Linear.Affine
import Data.Colour.SRGB (sRGB24read)

instance (FromJSON a) => FromJSON (Point V3 a) where
    parseJSON = fmap (\(x, y, z) -> P $ V3 x y z) . parseJSON

instance (FromJSON a) => FromJSON (V3 a) where
    parseJSON = fmap (\(x, y, z) -> V3 x y z) . parseJSON

instance (FromJSON a, Ord a, Floating a) => FromJSON (Colour a) where
  parseJSON = fmap sRGB24read . parseJSON

instance FromJSON Camera where
  parseJSON = withObject "camera" $ \v ->
    Camera <$> v.: "position"
           <*> v.: "direction"
           <*> v.: "up"

instance FromJSON Scene

instance FromJSON Object

instance FromJSON Shape

instance FromJSON Material

data Scene
  = Scene
    { objects    :: [Object]
    , lights     :: [Light]
    , camera     :: Camera
    , background :: Colour Double }
    deriving (Generic, Show)

instance FromJSON Light

data Light
  = AmbientLight
    { intensity :: Double }
  | PointLight
    { intensity :: Double
    , position  :: Point V3 Double }
  | DirectionalLight
    { intensity :: Double
    , direction :: V3 Double }
  deriving (Generic, Show)

data Shape
  = Sphere
    { sCenter     :: Point V3 Double
    , sRadius     :: Double }
  | Plane
    { planePoint  :: Point V3 Double
    , planeNormal :: V3 Double }
  | Triangle (Point V3 Double) (Point V3 Double) (Point V3 Double)
  deriving (Generic, Show)

data Material
  = ColorMaterial
    { color      :: Colour Double }
  | Material
    { color      :: Colour Double
    , reflective :: Double
    , specular   :: Double }
  deriving (Generic, Show)


data Object
  = Object
    { shape    :: Shape
    , material :: Material
    } deriving (Generic, Show)

data Camera
  = Camera
    { cameraPosition  :: Point V3 Double
    , cameraDirection :: V3 Double
    , cameraUp        :: V3 Double
    } deriving (Generic, Show)

loadScene :: FilePath -> IO (Either String Scene)
loadScene = eitherDecodeFileStrict
