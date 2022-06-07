-- |

module Hrt.Scene where

import Data.Colour
import Linear
import Linear.Affine

data Scene
  = Scene
    { objects    :: [Object]
    , lights     :: [Light]
    , camera     :: Camera
    , background :: Colour Double }
    deriving Show

data Light
  = AmbientLight
    { intensity :: Double }
  | PointLight
    { intensity :: Double
    , position  :: Point V3 Double }
  | DirectionalLight
    { intensity :: Double
    , direction :: V3 Double }
  deriving Show

data Shape
  = Sphere
    { sCenter     :: Point V3 Double
    , sRadius     :: Double }
  | Plane
    { planePoint  :: Point V3 Double
    , planeNormal :: V3 Double }
  | Triangle (Point V3 Double) (Point V3 Double) (Point V3 Double)
  deriving Show

data Material
  = ColorMaterial
    { color      :: Colour Double }
  | Material
    { color      :: Colour Double
    , reflective :: Double
    , specular   :: Double }
  deriving Show

data Object
  = Object
    { shape    :: Shape
    , material :: Material
    } deriving Show

data Camera
  = Camera
    { cameraPosition  :: Point V3 Double
    , cameraDirection :: V3 Double
    , cameraUp        :: V3 Double
    } deriving Show

data Ray
  = Ray
    { rayOrigin    :: Point V3 Double
    , rayDirection :: V3 Double
    } deriving Show

data Intersection
  = Intersection
    { intersectionPoint  :: Point V3 Double
    , intersectionNormal :: V3 Double
    , intersectionTMin   :: Double }
