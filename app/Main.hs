module Main where

import Data.Yaml (prettyPrintParseException)
import System.Environment (getArgs)
import System.Exit
import System.IO (hPutStrLn, stderr)

import Hrt
import Hrt.IO
import Hrt.Scene

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fp] -> do
      hPutStrLn stderr $ "Loading scene from " ++ fp
      scene <- loadScene fp
      case scene of
        Left s -> do
          die (prettyPrintParseException s)
        Right s -> do
          hPutStrLn stderr "Rendering to output.png"
          renderToPng s "output.png" canvasWidth canvasHeight
          hPutStrLn stderr "Done"
    _ -> die "USAGE: hrt <scenefile.json>"
