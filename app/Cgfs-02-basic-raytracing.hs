module Main where

import           Data.Colour
import qualified Data.Colour.Names as CN
import           Data.Colour.SRGB
import           Codec.Picture
import           Data.List (sortOn)
import           Linear

infinity :: Double
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
        let d' = b*b - 4*a*c in
          if d' < 0 then infinity else d'

      t1 = ((-1 * b) + sqrt discriminant) / (2*a)
      t2 = ((-1 * b) - sqrt discriminant) / (2*a)
  in (t1, t2)

-- |Compute the intersection of the ray (d) from its origin (o) with every
-- sphere and return the color of the sphere at the nearest intersection inside
-- the requested range of t.
traceRay :: Scene -> V3 Double -> V3 Double -> Double -> Double -> Colour Double
traceRay scene o d tMin tMax =
  let
    blarg s = [(t1, s), (t2, s)] where (t1, t2) = intersectRaySphere o d s
    blarg2 = sortOn fst [(t, s) | (t, s) <- concatMap blarg (spheres scene), t > tMin, t < tMax]
    closestSphere = if null blarg2 then Nothing else Just (snd $ head blarg2)
  in
    maybe backgroundColor sColor closestSphere

viewportSize :: Double
viewportSize = 1

projectionPlaneZ :: Double
projectionPlaneZ = 1

cameraPosition :: V3 Double
cameraPosition = V3 0 0 0

backgroundColor :: Colour Double
backgroundColor = black

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
  let scene = Scene [ Sphere (V3   0 (-1) 3) 1 CN.red
                    , Sphere (V3   2   0  4) 1 CN.blue
                    , Sphere (V3 (-2)  0  4) 1 CN.green
                    ]
  writePng "output.png" $ generateImage (pixelRenderer scene) canvasWidth canvasHeight
  putStrLn "wrote to output.png"
