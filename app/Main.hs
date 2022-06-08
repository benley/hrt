module Main where

import Codec.Picture
import System.Environment (getArgs)
import System.Exit

import Hrt
import Hrt.Scene

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fp] -> do
      putStrLn $ "Loading scene from " ++ fp
      scene <- loadScene fp
      case scene of
        Left s -> do
          putStrLn s
          exitFailure
        Right s -> do
          putStrLn "Rendering to output.png"
          writePng "output.png" $ generateImage (pixelRenderer s) canvasWidth canvasHeight
          putStrLn "Done"
    _ -> die "USAGE: hrt <scenefile.json>"
