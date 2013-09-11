module Main where

import Control.Monad.Error (runErrorT)
import Text.Deiko.Config

main = either handler go =<< runErrorT action
  where
    handler = error . show . configMsg
    go _    = putStrLn "Config loaded"
    action  = loadFile "tests/resources/app.conf"
