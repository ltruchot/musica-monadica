module Main (main) where

import           Data.Text.Lazy.IO as TextLazy (putStrLn)
import           Lib               (generateMX)

main :: IO ()
main = TextLazy.putStrLn $ generateMX ()
