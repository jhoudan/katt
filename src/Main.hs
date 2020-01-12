{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

import            Turtle hiding (err)

import            GHC.Generics (Generic)
import            Data.Aeson (FromJSON, decode)
import            Data.Maybe (fromMaybe)
import            Data.Either (fromRight)
import qualified  Data.ByteString.Lazy as B
import            Data.Text.Lazy (fromStrict)
import            Data.String.Conversions (cs)

import            Control.Monad (when)

import            Network.Mail.Mime (simpleMail', Mail, Address(..))
import            Network.Mail.Mime.SES (renderSendMailSESGlobal, SES(..))

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

curlPage :: Text -> Shell (Either Text Text)
curlPage url' = do
  (exitCode, out, err) <- procStrictWithErr "curl" [ "--silent", url' ] empty
  case exitCode of
    ExitSuccess -> return $ Right out
    _           -> return $ Left err

-- Repeatedly fetch the content of the page
-- and check if there are differences with version n - 1
-- If the first curl fails, the page is considered to be empty
checkPage :: Config -> Shell ()
checkPage config = do
  eBasePage <- curlPage (url config)
  recCheckPage config (fromRight "" eBasePage)

recCheckPage :: Config -> Text -> Shell ()
recCheckPage config basePage = do
  ePage <- curlPage (url config)
  case ePage of
    Right page  -> when (page /= basePage) (liftIO $ sendMail config)
    Left err    -> liftIO $ print ("cURL failed: " <> err)
  sleep (60.0 * 5.0)      -- wait for 5 minutes
  recCheckPage config (fromRight basePage ePage)

helpMessage :: Text
helpMessage = "Missing or invalid config file - please refer to the README.md, or to config.json.init for more information"

main :: IO ()
main = do
  configRaw <- B.readFile "config.json"
  case decode configRaw of
    Nothing     -> print helpMessage
    Just config -> sh $ checkPage config
