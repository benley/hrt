module Main where

import Data.Colour (Colour, black)
import qualified Data.Colour.Names as CN
import Linear
import Data.List (sortBy, sortOn)

infinity = 1/0

newtype Scene = Scene
  { spheres :: [Sphere]
  }

data Sphere = Sphere
  { sCenter :: V3 Double
  , sRadius :: Double
  , sColor  :: Colour Double
  }

-- | Compute the intersection(s) of a ray and a sphere
intersectRaySphere :: V3 Double -> V3 Double -> Sphere -> (Double, Double)
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

-- |Compute the intersection of the ray (d) from its origin (o) with every
-- sphere and return the color of the sphere at the nearest intersection inside
-- the requested range of t.  Probably broken.
traceRay :: Scene -> V3 Double -> V3 Double -> Double -> Double -> Colour Double
traceRay scene o d tMin tMax =
  let
    blarg s = [(t1, s), (t2, s)] where (t1, t2) = intersectRaySphere o d s
    blarg2 = sortOn fst [(t, s) | (t, s) <- concatMap blarg (spheres scene), t > tMin, t < tMax]
    closestSphere = if null blarg2 then Nothing else Just (snd $ head blarg2)
  in
    maybe backgroundColor sColor closestSphere

viewportSize = 1

projectionPlaneZ = 1

cameraPosition :: V3 Double
cameraPosition = V3 0 0 0

backgroundColor :: Colour Double
backgroundColor = black

canvasWidth = 800
canvasHeight = 600

-- | Convert 2D canvas coordinates to 3D viewport coordinates
canvasToViewport :: V2 Double -> V3 Double
canvasToViewport (V2 x y) =
  V3 (x * viewportSize / canvasWidth) (y * viewportSize / canvasHeight) projectionPlaneZ

main :: IO ()
main = do
  let scene = Scene [ Sphere (V3   0 (-1) 3) 1 CN.red
                    , Sphere (V3   2   0  4) 1 CN.blue
                    , Sphere (V3 (-2)  0  4) 1 CN.green ]
  undefined
