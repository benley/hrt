module Main where

import Codec.Picture
import Linear
import Linear.Affine
import qualified Data.Colour.Names as CN

import Hrt
import Hrt.Scene

demoScene :: Scene
demoScene =
  Scene
  { objects =
      [ Object
        { shape = Sphere
                  { sCenter = P $ V3 0 (-1) 3
                  , sRadius = 1 }
        , material = Material
                     { color = CN.red
                     , specular = 500
                     , reflective = 0.2 } }
      , Object
        { shape = Sphere
                  { sCenter = P $ V3 (-2) 0 4
                  , sRadius = 1 }
        , material = Material
                     { color = CN.green
                     , specular = 10
                     , reflective = 0.2 } }
      , Object
        { shape = Sphere
                  { sCenter = P $ V3 2 0 4
                  , sRadius = 1 }
        , material = Material
                     { color = CN.blue
                     , specular = 500
                     , reflective = 0.1 } }
      , Object
        { shape = Plane
                  { planePoint = P $ V3 0 (-1) 0
                  , planeNormal = V3 0 1 0 }
        , material = Material
                     { color = CN.yellow
                     , specular = 1000
                     , reflective = 0.5 } }
      , Object
        { shape = Triangle (P$V3 (-1) 0.5 4) (P$V3 1 0.5 4) (P$V3 0 1.25 2.5)
        , material = Material
                     { color = CN.silver
                     , specular = 0
                     , reflective = 1.0 } }
      ]
  , lights =
      [ AmbientLight 0.05
      , PointLight 0.6 (P $ V3 2 1 0)
      , DirectionalLight 0.2 (V3 1 4 4) ]
  , camera =
      Camera
      { cameraPosition = P $ V3 0 0 0
      , cameraDirection = V3 0 0 (-1)
      , cameraUp = V3 0 1 0 }
  , background = CN.black }

main :: IO ()
main = do
  writePng "output.png" $ generateImage (pixelRenderer demoScene) canvasWidth canvasHeight
  putStrLn "wrote to output.png"
