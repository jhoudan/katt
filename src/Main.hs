{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

import            GHC.Generics (Generic)
import            Data.Text (Text)
import            Data.Maybe (fromMaybe, fromJust)
import qualified  Data.ByteString.Lazy as B
import            Data.Text.Lazy (fromStrict)
import            Data.String.Conversions (cs)

import            Text.URI as URI
import            Network.HTTP.Req
import            Data.Aeson (FromJSON, decode)

import            Control.Monad (when)
import            Control.Concurrent (threadDelay)

import            Network.Mail.Mime (simpleMail', Mail, Address(..))
import            Network.Mail.Mime.SES (renderSendMailSESGlobal, SES(..))

import            System.Environment
import            System.IO (hPutStr, stderr)

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

fetchPage :: Text -> IO Text
fetchPage url' = do
  uri <- URI.mkURI url'
  runReq defaultHttpConfig $ do
    let (parsedUrl, options) = fromJust (useHttpsURI uri)
    response <- req GET parsedUrl NoReqBody bsResponse options
    return $ cs . responseBody $ response

-- Repeatedly fetch the content of the page
-- and check if there are differences with version n - 1
-- If the first curl fails, the page is considered to be empty
checkPage :: Config -> IO ()
checkPage config = do
  basePage <- fetchPage (url config)
  recCheckPage config basePage

recCheckPage :: Config -> Text -> IO ()
recCheckPage config basePage = do
  page <- fetchPage (url config)
  when (page /= basePage) (sendMail config)
  threadDelay 300000000 -- 5 minutes
  recCheckPage config page

helpMessage :: String
helpMessage = "Error: Missing or invalid config file.\nPlease refer to the README.md, or to config.json.init for more information"

getConfig :: [FilePath] -> IO (Maybe Config)
getConfig [] = return $ Nothing
getConfig (x:_) = do
  configRaw <- B.readFile x
  return $ decode configRaw

main :: IO ()
main = do
  args <- getArgs
  mConfig <- getConfig args
  case mConfig of
    Nothing     -> hPutStr stderr helpMessage
    Just config -> checkPage config
