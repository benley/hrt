module Hrt.IO where

import Codec.Picture (writePng, generateImage)

import Hrt
import Hrt.Scene
import qualified Data.Yaml as Yaml

loadScene :: FilePath -> IO (Either Yaml.ParseException Scene)
loadScene = Yaml.decodeFileEither

renderToPng :: Scene -> FilePath -> Int -> Int -> IO ()
renderToPng s fp width height =
  writePng fp $ generateImage (pixelRenderer s) width height
