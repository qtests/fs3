-- {-# START_FILE Handler/Home.hs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- show
module Handler.Home where

import Data.Default
import Yesod
import Yesod.Default.Util
import Data.Conduit
import Data.Conduit.Binary
-- Older version of Data.Conduit might have exported ResourceT directly, 
-- but nowadays you need to import it explicitly with
import Control.Monad.Trans.Resource

import Foundation

getHomeR :: Handler Html
getHomeR = do
    -- let filenames = ["readme.txt", "report.pdf", "music.wav"] :: [String]
    (formWidget, formEncType) <- generateFormPost uploadForm
    filenames <- getList
    defaultLayout $ do
        setTitle "File Processor"
        $(widgetFileNoReload def "home")

postHomeR :: Handler Html
postHomeR = do
    ((result, _), _) <- runFormPost uploadForm
    case result of
      FormSuccess fi -> do
        app <- getYesod
        -- addFile app $ fileName fi
        fileBytes <- runResourceT $ fileSource fi $$ sinkLbs
        addFile app (fileName fi, fileBytes)
      _ -> return ()
    redirect HomeR

uploadForm = renderDivs $ fileAFormReq "file"

