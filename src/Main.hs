{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

import            Turtle

import            GHC.Generics (Generic)
import            Data.Aeson (FromJSON, decode)
import            Data.Maybe (fromMaybe)
import qualified  Data.ByteString.Lazy as B
import            Data.Text.Lazy (fromStrict)
import            Data.String.Conversions (cs)

import            Control.Monad (when)
import qualified  Control.Foldl as Fold

import            Network.Mail.Mime
import            Network.Mail.Mime.SES

data Config = Config { from             :: Text
                     , to               :: [Text]
                     , url              :: Text
                     , awsAccessKeyId   :: Text
                     , awsSecretAccess  :: Text
                     , region           :: Maybe Text
                     } deriving (Generic, FromJSON)

-- TODO error handling
curlPage :: Text -> Shell Text
curlPage url = do
  let pageStream = inproc "curl" [ "--silent", url ] empty
  page <- liftIO $ fold pageStream Fold.mconcat
  return (lineToText page)

mailPayload :: Config -> Mail
mailPayload config = simpleMail' sender receiver subject body
  where sender    = Address Nothing (cs $ from config)
        receiver  = Address Nothing (cs . head . to $ config) -- TODO should be the list of all receivers
        subject   = "Changes on " <> (url config)
        body      = fromStrict $ "New changes detected, go check " <> (url config)

sendMail :: Config -> IO ()
sendMail config = renderSendMailSESGlobal sesPayload (mailPayload config)
  where sesPayload = SES { sesFrom          = cs . from $ config
                          , sesTo           = map cs . to $ config
                          , sesAccessKey    = cs . awsAccessKeyId $ config
                          , sesSecretKey    = cs . awsSecretAccess $ config
                          , sesSessionToken = Nothing
                          , sesRegion       = fromMaybe "us-east-1" (region config) }

-- 1 - Load first version of the page
-- 2 - Load new version of the page
--     > if different, send mail
-- 3 - Recurse step 2 with latest version of the page
-- TODO Use something like forever instead of recursion?
checkPage :: Config -> Shell ()
checkPage config = curlPage (url config) >>= recCheckPage config

recCheckPage :: Config -> Text -> Shell ()
recCheckPage config basePage = do
  page <- curlPage (url config)
  when (page /= basePage) (liftIO $ sendMail config)
  sleep (60.0 * 5.0)      -- wait for 5 minutes
  recCheckPage config page

main :: IO ()
main = do
  configRaw <- B.readFile "config.json"
  case decode configRaw of
    Nothing     -> print "Missing or invalid config file"
    Just config -> sh $ checkPage config
