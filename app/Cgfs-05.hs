{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Control.Lens
import Data.Colour
import Data.Colour.SRGB
import qualified Data.Colour.Names as CN
import Linear
import Linear.Affine
import Data.List (sortOn)
import Codec.Picture

infinity :: Double
infinity = 1/0

epsilon :: Double
epsilon = 1e-07

data Scene
  = Scene
    { shapes :: [Shape]
    , lights  :: [Light]
    , camera  :: Camera }
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
    , sRadius     :: Double
    , sColor      :: Colour Double
    , sSpecular   :: Double
    , sReflective :: Double }
  | Plane
    { planePoint  :: Point V3 Double
    , planeNormal :: V3 Double
    }
  deriving Show

data Camera
  = Camera
    { cameraPosition  :: Point V3 Double
    , cameraDirection :: V3 Double
    , cameraUp        :: V3 Double
    } deriving Show

data Ray
  = Ray
    { rayOrigin :: Point V3 Double
    , rayDirection :: V3 Double
    } deriving Show

-- | Compute light intensity at a point & normal
computeLighting
  :: Scene
  -> Point V3 Double -- ^ point
  -> V3 Double       -- ^ normal
  -> V3 Double       -- ^ camera vector
  -> Double          -- ^ specular exponent
  -> Double
computeLighting scene p n cV s
  = sum $ map computeLight $ lights scene
  where
    computeLight (AmbientLight i) = i

    computeLight (PointLight {intensity, position}) = do
      let l = unP $ position - p
      case closestIntersection scene (Ray p l) epsilon 1 of
        -- No shadow
        Nothing ->
          if dot n l <= 0 then 0 else
            intensity * dot n l / (norm n * norm l) + doSpecular intensity l
        -- Light ray is occluded
        Just _ -> 0

    computeLight (DirectionalLight {intensity, direction}) = do
      let l = direction
      case closestIntersection scene (Ray p l) epsilon infinity of
        Nothing ->
          if dot n l <= 0 then 0 else
            intensity * dot n l / (norm n * norm l) + doSpecular intensity l
        Just _ -> 0

    doSpecular :: Double -> V3 Double -> Double
    doSpecular intensity l | s >= 0 =
                             let r = reflectRay l n in
                               if dot r cV > 0 then
                                 intensity * ((dot r cV / (norm r * norm cV)) ** s)
                               else 0
                           | otherwise = 0

reflectRay
  :: V3 Double  -- ^ direction
  -> V3 Double  -- ^ normal
  -> V3 Double  -- ^ reflected direction
reflectRay r n = 2 *^ n ^* dot n r - r

-- | Compute the intersection(s) of a ray and a shape
intersectRayShape
  :: Ray
  -> Shape
  -> (Double, Double)
intersectRayShape (Ray o d) (Sphere {sRadius=r, sCenter}) =
  let co = unP $ o - sCenter
      a = dot d d
      b = 2 * dot co d
      c = dot co co - (r * r)

      discriminant =
        if dsc < 0 then infinity else dsc
        where dsc = b*b - 4*a*c

      t1 = ((-1 * b) + sqrt discriminant) / (2*a)
      t2 = ((-1 * b) - sqrt discriminant) / (2*a)
  in (t1, t2)

-- | Find the nearest intersection between a ray and any object
closestIntersection
  :: Scene
  -> Ray
  -> Double                 -- ^ min distance
  -> Double                 -- ^ max distance
  -> Maybe (Double, Shape)
closestIntersection scene ray tMin tMax =
  let
    -- I am sorry for this mess
    blarg s = [(t1, s), (t2, s)] where (t1, t2) = intersectRayShape ray s
    allIntersections = sortOn fst [(t, s) | (t, s) <- concatMap blarg (shapes scene), t >= tMin, t < tMax]
    (closestT, closestSphere) = head allIntersections
  in
    if null allIntersections then Nothing else Just (closestT, closestSphere)

-- | Trace a ray from the origin in some direction, find the first object it
-- hits (if any), and return the object's color after acccounting for lighting.
traceRay
  :: Scene
  -> Ray
  -> Double           -- ^ min distance
  -> Double           -- ^ max distance
  -> Int              -- ^ recursion limit
  -> Colour Double
traceRay scene ray@(Ray (P o) d) tMin tMax rl =
  case closestIntersection scene ray tMin tMax of
    Nothing -> backgroundColor
    Just (closestT, closestSphere) ->
      let
        intersection = P $ o + (closestT *^ d)
        normal = n ^/ norm n where P n = intersection - sCenter closestSphere
        intensity = computeLighting scene intersection normal (-d) (sSpecular closestSphere)
        localColor = darken intensity (sColor closestSphere)

        reflectedColor =
          traceRay scene (Ray intersection (reflectRay (-d) normal)) epsilon infinity (rl - 1)
        r = sReflective closestSphere
      in
        if rl <= 0 || r <= 0 then
          localColor
        else
          blend r reflectedColor localColor

viewportSize :: Double
viewportSize = 1

-- | Distance between the camera and the projection plane
projectionPlaneZ :: Double
projectionPlaneZ = 1

backgroundColor :: Colour Double
backgroundColor = CN.black

canvasWidth :: Int
canvasWidth = 800
canvasHeight :: Int
canvasHeight = 800

-- | Convert 2D canvas coordinates to 3D viewport coordinates
canvasToViewport :: V2 Int -> V3 Double
canvasToViewport (V2 x y) =
  V3 (     fromIntegral (x - (canvasWidth  `div` 2)) * viewportSize / fromIntegral canvasWidth)
     (-1 * fromIntegral (y - (canvasHeight `div` 2)) * viewportSize / fromIntegral canvasHeight)
     projectionPlaneZ

-- | reflected light recursion limit
recursionDepth :: Int
recursionDepth = 3

pixelRenderer :: Scene -> Int -> Int -> PixelRGB8
pixelRenderer scene@Scene{camera} x y =
  let
    cameraRotation :: M33 Double
    cameraRotation = lookAt (unP $ cameraPosition camera) (cameraDirection camera) (cameraUp camera) ^. _m33
    direction = cameraRotation !* canvasToViewport (V2 x y)
    RGB r g b = toSRGB24 $ traceRay scene (Ray (cameraPosition camera) direction) epsilon infinity recursionDepth
  in PixelRGB8 r g b

demoScene :: Scene
demoScene =
  Scene
  { shapes =
      [ Sphere
        { sCenter = P $ V3 0 (-1) 3
        , sRadius = 1
        , sColor = CN.red
        , sSpecular = 500
        , sReflective = 0.2 }
      , Sphere
        { sCenter = P $ V3 (-2) 0 4
        , sRadius = 1
        , sColor = CN.green
        , sSpecular = 10
        , sReflective = 0.2 }
      , Sphere
        { sCenter = P $ V3 2 0 4
        , sRadius = 1
        , sColor = CN.blue
        , sSpecular = 500
        , sReflective = 0.1 }
      , Sphere
        { sCenter = P $ V3 0 (-5001) 0
        , sRadius = 5000
        , sColor = CN.yellow
        , sSpecular = 1000
        , sReflective = 0.5 } ]
  , lights =
      [ AmbientLight 0.05
      , PointLight 0.6 (P $ V3 2 1 0)
      , DirectionalLight 0.2 (V3 1 4 4) ]
  , camera =
      Camera
      { cameraPosition = P $ V3 0 0 0
      , cameraDirection = V3 0 0 (-1)
      , cameraUp = V3 0 1 0 } }

main :: IO ()
main = do
  writePng "output.png" $ generateImage (pixelRenderer demoScene) canvasWidth canvasHeight
  putStrLn "wrote to output.png"
