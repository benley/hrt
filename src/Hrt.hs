{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Hrt where

import Codec.Picture
import Control.Lens
import Data.Colour
import Data.Colour.SRGB
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Linear
import Linear.Affine

import Hrt.Scene

infinity :: Double
infinity = 1/0

epsilon :: Double
epsilon = 1e-07

-- | Compute light intensity at a point & normal
computeLighting
  :: Scene
  -> Point V3 Double -- ^ point
  -> V3 Double       -- ^ normal
  -> V3 Double       -- ^ camera vector
  -> Double          -- ^ specular exponent
  -> Double
computeLighting scene p n cV s
  = sum $ map computeLight scene.lights
  where
    computeLight (AmbientLight i) = i

    computeLight light@PointLight{} = do
      let l = unP $ light.position - p
          nDotL = n `dot` l
      case closestIntersection scene (Ray p l) epsilon 1 of
        Nothing | nDotL > 0 ->
          -- No shadow, and the light hits the front surface
          light.intensity * nDotL / (norm n * norm l) + doSpecular light.intensity l
        _ -> 0

    computeLight light@DirectionalLight{} = do
      let nDotL = n `dot` light.direction
      case closestIntersection scene (Ray p light.direction) epsilon infinity of
        Nothing | nDotL > 0 ->
            light.intensity * nDotL / (norm n * norm light.direction) + doSpecular light.intensity light.direction
        _ -> 0

    doSpecular :: Double -> V3 Double -> Double
    doSpecular _ _ | s < 0 = 0
    doSpecular intensity l = do
      let r = reflectRay l n
          rDotCV = r `dot` cV
      if rDotCV > 0 then
        intensity * ((rDotCV / (norm r * norm cV)) ** s)
      else 0

reflectRay
  :: V3 Double  -- ^ direction
  -> V3 Double  -- ^ normal
  -> V3 Double  -- ^ reflected direction
reflectRay r n = 2 *^ n ^* dot n r - r

-- | Compute the intersection(s) of a ray and a shape
intersectRayShape
  :: Ray
  -> Shape
  -> Maybe Intersection
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
  in
    if discriminant == infinity
    then Nothing
    else do
      -- TODO: if tMin < epsilon then we should return the other intersection
      --       point (unless it too is < epsilon)
      let tMin = min t1 t2
          intersection = P (unP o + (tMin *^ d))
          normal = n ^/ norm n where P n = intersection - sCenter
      Just Intersection { intersectionPoint = intersection
                        , intersectionNormal = normal
                        , intersectionTMin = tMin }

intersectRayShape (Ray{rayOrigin, rayDirection}) (Plane{planePoint, planeNormal}) = do
  let denominator = dot rayDirection planeNormal
      t = unP (planePoint - rayOrigin) `dot` (planeNormal ^/ denominator)
  if abs denominator <= epsilon || t <= epsilon
    then Nothing
    else Just Intersection { intersectionPoint = rayOrigin .+^ (rayDirection ^* t)
                           , intersectionNormal = planeNormal
                           , intersectionTMin = t }

intersectRayShape ray (Triangle tA tB tC) = do
  let planeNormal = unP (tB - tA) `cross` unP (tC - tA)
  case intersectRayShape ray (Plane tA planeNormal) of
    Nothing -> Nothing
    Just i@Intersection{intersectionPoint} -> do
      let edge0 = unP $ tB - tA; edge1 = unP $ tC - tB; edge2 = unP $ tA - tC
      if    planeNormal `dot` (edge0 `cross` unP (intersectionPoint - tA)) >= 0
         && planeNormal `dot` (edge1 `cross` unP (intersectionPoint - tB)) >= 0
         && planeNormal `dot` (edge2 `cross` unP (intersectionPoint - tC)) >= 0
      then Just i
      else Nothing

-- | Find the nearest intersection between a ray and any object
closestIntersection
  :: Scene
  -> Ray
  -> Double                 -- ^ min distance
  -> Double                 -- ^ max distance
  -> Maybe (Intersection, Object)
closestIntersection scene ray tMin tMax =
  let
    -- this is still clumsy
    blarg :: Object -> Maybe (Intersection, Object)
    blarg obj = case intersectRayShape ray (shape obj) of
                    Just i@Intersection{intersectionTMin=t} | t < tMax && t >= tMin -> Just (i, obj)
                    _                                                               -> Nothing
    allIntersections :: [(Intersection, Object)]
    allIntersections = sortOn (\(Intersection{intersectionTMin=t}, _) -> t) (mapMaybe blarg (objects scene))
  in case allIntersections of
    a:_ -> Just a
    _   -> Nothing

-- | Trace a ray from the origin in some direction, find the first object it
-- hits (if any), and return the object's color after acccounting for lighting.
traceRay
  :: Scene
  -> Ray
  -> Double           -- ^ min distance
  -> Double           -- ^ max distance
  -> Int              -- ^ recursion limit
  -> Colour Double
traceRay scene ray@(Ray _ d) tMin tMax rl =
  case closestIntersection scene ray tMin tMax of
    Nothing -> background scene
    Just (closestI, closestShape) ->
      let
        intersection = intersectionPoint closestI
        normal = intersectionNormal closestI
        intensity = computeLighting scene intersection normal (-d) (specular (material closestShape))
        localColor = darken intensity (color (material closestShape))

        reflectedColor =
          traceRay scene (Ray intersection (reflectRay (-d) normal)) epsilon infinity (rl - 1)
        r = reflective (material closestShape)
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
