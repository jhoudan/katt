{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

import            GHC.Generics (Generic)
import            Data.Text (Text)
import            Data.Aeson (FromJSON, decode)
import            Data.Maybe (fromMaybe)
import qualified  Data.ByteString.Lazy as B
import            Data.Text.Lazy (fromStrict)
import            Data.String.Conversions (cs)
import            Network.HTTP.Simple (httpBS, getResponseBody, parseRequest)

import            Control.Monad (when)
import            Control.Concurrent (threadDelay)

import            Network.Mail.Mime (simpleMail', Mail, Address(..))
import            Network.Mail.Mime.SES (renderSendMailSESGlobal, SES(..))

import            System.Environment

data Config = Config { from             :: Text
                     , to               :: [Text]
                     , url              :: Text
                     , awsAccessKeyId   :: Text
                     , awsSecretAccess  :: Text
                     , region           :: Maybe Text
                     } deriving (Generic, FromJSON)

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

-- TODO error handling, check response code
fetchPage :: Text -> IO Text
fetchPage url' = do
  request <- parseRequest (cs url')
  response <- httpBS request
  return $ cs . getResponseBody $ response

-- Repeatedly fetch the content of the page
-- and check if there are differences with version n - 1
-- If the first curl fails, the page is considered to be empty
checkPage :: Config -> IO ()
checkPage config = do
  basePage <- fetchPage (url config)
  recCheckPage config basePage

recCheckPage :: Config -> Text -> IO ()
recCheckPage config basePage = do
  print "Checking page..."
  page <- fetchPage (url config)
  when (page /= basePage) (print "Something changed!" >> sendMail config)
  threadDelay 300000000 -- 5 minutes
  recCheckPage config page

helpMessage :: Text
helpMessage = "Missing or invalid config file - please refer to the README.md, or to config.json.init for more information"

-- TODO error handling on args
main :: IO ()
main = do
  args <- getArgs
  configRaw <- B.readFile (args !! 0)
  case decode configRaw of
    Nothing     -> print helpMessage
    Just config -> checkPage config
