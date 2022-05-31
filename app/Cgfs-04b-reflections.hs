{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Data.Colour
import Data.Colour.SRGB
import qualified Data.Colour.Names as CN
import Linear
import Data.List (sortBy, sortOn)
import Codec.Picture
import Data.Maybe (fromJust)
import qualified Data.Colour as CN

infinity = 1/0

data Scene
  = Scene
    { spheres :: [Sphere]
    , lights :: [Light]
    }

data Light
  = Ambient
    { intensity :: Double }
  | Point
    { intensity :: Double
    , position :: V3 Double }
  | Directional
    { intensity :: Double
    , direction :: V3 Double }

data Sphere
  = Sphere
    { sCenter :: V3 Double
    , sRadius :: Double
    , sColor  :: Colour Double
    , sSpecular :: Double
    }

-- | Compute light intensity at a point & normal
computeLighting
  :: Scene
  -> V3 Double -- ^ point
  -> V3 Double -- ^ normal
  -> V3 Double -- ^ camera vector
  -> Double    -- ^ specular exponent
  -> Double
computeLighting scene p n cV s
  = sum $ map computeLight $ lights scene
  where
    computeLight (Ambient i) = i

    computeLight (Point {intensity, position}) = do
      let l = position - p
      case closestIntersection scene p l 0.001 1 of
        Nothing ->                           -- No shadow
          if dot n l <= 0 then 0 else
            intensity * (dot n l) / (norm n * norm l) + doSpecular intensity l
        Just _ -> 0                          -- Light ray is occluded

    computeLight (Directional {intensity, direction}) = do
      let l = direction
      case closestIntersection scene p l 0.001 infinity of
        Nothing ->
          if dot n l <= 0 then 0 else
            intensity * (dot n l) / (norm n * norm l) + doSpecular intensity l
        Just _ -> 0

    doSpecular intensity l
      | s >= 0 =
          let r = 2 *^ n ^* (dot n l) - l in
            if dot r cV > 0 then
              intensity * ((dot r cV / (norm r * norm cV)) ** s)
            else 0
      | otherwise = 0

-- | Compute the intersection(s) of a ray and a sphere
intersectRaySphere
  :: V3 Double -- ^ origin
  -> V3 Double -- ^ direction
  -> Sphere
  -> (Double, Double)
intersectRaySphere o d sphere =
  let r = sRadius sphere
      co = o - sCenter sphere
      a = dot d d
      b = 2 * dot co d
      c = dot co co - (r * r)

      discriminant =
        let d = b*b - 4*a*c in
          if d < 0 then infinity else d

      t1 = ((-1 * b) + sqrt discriminant) / (2*a)
      t2 = ((-1 * b) - sqrt discriminant) / (2*a)
  in (t1, t2)


-- | Find the nearest intersection with any object from an origin point in some direction
closestIntersection
  :: Scene
  -> V3 Double -- ^ origin
  -> V3 Double -- ^ direction
  -> Double    -- ^ min distance
  -> Double    -- ^ max distance
  -> Maybe (Double, Sphere)
closestIntersection scene o d tMin tMax =
  let
    -- I am sorry for this mess
    blarg s = [(t1, s), (t2, s)] where (t1, t2) = intersectRaySphere o d s
    allIntersections = sortOn fst [(t, s) | (t, s) <- concatMap blarg (spheres scene), t >= tMin, t < tMax]
    (closestT, closestSphere) = head allIntersections
  in
    if null allIntersections then Nothing else Just (closestT, closestSphere)

-- | Trace a ray from the origin in some direction, find the first object it
-- hits (if any), and return the object's color after acccounting for lighting.
traceRay
  :: Scene
  -> V3 Double     -- ^ origin
  -> V3 Double     -- ^ direction
  -> Double        -- ^ min distance
  -> Double        -- ^ max distance
  -> Colour Double
traceRay scene o d tMin tMax =
  case closestIntersection scene o d tMin tMax of
    Nothing -> backgroundColor
    Just (closestT, closestSphere) ->
      let
        intersection = o + (closestT *^ d)
        normal = n ^/ norm n where n = intersection - sCenter closestSphere
        intensity = computeLighting scene intersection normal (-d) (sSpecular closestSphere)
      in darken intensity (sColor closestSphere)

viewportSize = 1

projectionPlaneZ = 1

cameraPosition :: V3 Double
cameraPosition = V3 0 0 0

backgroundColor :: Colour Double
backgroundColor = CN.white

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

pixelRenderer :: Scene -> Int -> Int -> PixelRGB8
pixelRenderer scene x y =
  let direction = canvasToViewport (V2 x y)
      color     = toSRGB24 $ traceRay scene cameraPosition direction 1 infinity
      (r, g, b) = (channelRed color, channelGreen color, channelBlue color)
  in PixelRGB8 r g b

main :: IO ()
main = do
  let scene =
        Scene { spheres = [ Sphere (V3   0    (-1) 3) 1    CN.red    500
                          , Sphere (V3   2      0  4) 1    CN.blue   500
                          , Sphere (V3 (-2)     0  4) 1    CN.green  10
                          , Sphere (V3   0 (-5001) 0) 5000 CN.yellow 1000
                          ]
              , lights = [ Ambient 0.05
                         , Point 0.6 (V3 2 1 0)
                         , Directional 0.2 (V3 1 4 4)
                         ]
              }
  writePng "output.png" $ generateImage (pixelRenderer scene) canvasWidth canvasHeight
  putStrLn "wrote to output.png"
