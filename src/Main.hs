{-# LANGUAGE OverloadedStrings #-}

-- https://github.com/ShaneKilkelly/YesodExample

module Main where

import Yesod
import Control.Concurrent.STM

import Dispatch ()
import Foundation
  
-- http://localhost:3000/
main :: IO ()
-- main = warp 3000 App
-- main = warp 3000 $ App ["readme.txt", "report.pdf", "music.wav"]
main = do
    -- tfilenames <- atomically $ newTVar ["readme.txt", "report.pdf", "music.wav"]
    tfilenames <- atomically $ newTVar []
    warp 3000 $ App tfilenames


