{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.Deiko.Config

main = go =<< action
  where
    go _   = putStrLn "Config loaded"
    action = loadFile "tests/resources/app.conf"
