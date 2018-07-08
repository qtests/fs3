
-- {-# START_FILE Dispatch.hs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- show
module Dispatch where

import Data.Default
import Yesod
import Yesod.Default.Util

import Handler.Home
import Handler.Preview
import Foundation

mkYesodDispatch "App" resourcesApp

{- getHomeR :: Handler Html
getHomeR = do
    let filenames = ["readme.txt", "report.pdf", "music.wav"] :: [String]
    defaultLayout $ do
        setTitle "File Processor"
        $(widgetFileNoReload def "home") -}
-- /show