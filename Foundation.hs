-- {-# START_FILE Foundation.hs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
-- https://www.schoolofhaskell.com/user/PthariensFlame/guide-to-ghc-extensions/pattern-and-guard-extensions#viewpatterns-as-a-replacement-for-npluskpatterns


-- show
module Foundation where

import Data.Text
import Control.Concurrent.STM
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text as Text

import Yesod

-- data App = App
-- data App = App [Text]
-- data App = App (TVar [Text])
data App = App (TVar [(Text, ByteString)])

instance Yesod App

instance RenderMessage App FormMessage where
   renderMessage _ _ = defaultFormMessage

mkYesodData "App" $(parseRoutesFile "config/routes")

getList :: Handler [Text]
getList = do
--     App state <- getYesod
--     return state
   App tstate <- getYesod
   -- liftIO $ readTVarIO tstate
   state <- liftIO $ readTVarIO tstate
   return $ fmap fst state

-- addFile :: App -> Text -> Handler ()
addFile :: App -> (Text, ByteString) -> Handler ()
addFile (App tstore) op =
    liftIO . atomically $ do
    modifyTVar tstore $ \ ops -> op : ops


getById :: Text -> Handler ByteString
getById ident = do
    App tstore <- getYesod
    operations <- liftIO $ readTVarIO tstore
    case lookup ident operations of
      Nothing -> notFound
      Just bytes -> return bytes
