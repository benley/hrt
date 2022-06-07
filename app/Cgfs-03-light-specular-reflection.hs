{-# LANGUAGE NamedFieldPuns #-}
module Main where

import           Codec.Picture
import           Data.Colour
import qualified Data.Colour.Names as CN
import           Data.Colour.SRGB
import           Data.List (sortOn)
import           Linear

infinity :: Double
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
computeLighting (Scene {lights}) p n cV s
  = sum $ map computeLight lights
  where
    computeLight (Ambient i) = i

    computeLight (Point {intensity, position}) =
      if dot n l <= 0 then 0 else
        intensity * dot n l / (norm n * norm l) + doSpecular intensity l
      where l = position - p

    computeLight (Directional {intensity, direction}) =
      if dot n l <= 0 then 0 else
        intensity * dot n l / (norm n * norm l) + doSpecular intensity l
      where l = direction

    doSpecular intensity l
      | s >= 0 =
          let r = 2 *^ n ^* dot n l - l in
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
        let d' = b*b - 4*a*c in
          if d' < 0 then infinity else d'

      t1 = ((-1 * b) + sqrt discriminant) / (2*a)
      t2 = ((-1 * b) - sqrt discriminant) / (2*a)
  in (t1, t2)

-- |Compute the intersection of the ray (d) from its origin (o) with every
-- sphere and return the color of the sphere at the nearest intersection inside
-- the requested range of t.
traceRay
  :: Scene
  -> V3 Double     -- ^ origin
  -> V3 Double     -- ^ direction
  -> Double        -- ^ min distance
  -> Double        -- ^ max distance
  -> Colour Double
traceRay scene o d tMin tMax =
  let
    -- I am sorry for this mess
    blarg s = [(t1, s), (t2, s)] where (t1, t2) = intersectRaySphere o d s
    allIntersections = sortOn fst [(t, s) | (t, s) <- concatMap blarg (spheres scene), t >= tMin, t < tMax]
    (closestT, closestSphere) = head allIntersections

    intersection = o + (closestT *^ d)
    normal = n ^/ norm n where n = intersection - sCenter closestSphere -- sphere normal at intersection
  in
    if null allIntersections then backgroundColor
    else
      let intensity = computeLighting scene intersection normal (-d) (sSpecular closestSphere)
      in darken intensity (sColor closestSphere)

viewportSize :: Double
viewportSize = 1

projectionPlaneZ :: Double
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
