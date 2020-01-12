{-# LANGUAGE OverloadedStrings #-}

module Main where

import            Turtle
import            Control.Monad (when)
import qualified  Control.Foldl as Fold

url :: Text
url = "https://www.chatteriedescoonberries.com/chatons.html"

-- TODO Check if error to avoid 1) crash 2) false alarm
curlPage :: Text -> Shell Text
curlPage url = do
  let pageStream = inproc "curl" [ "--silent", url ] empty
  page <- liftIO $ fold pageStream Fold.mconcat
  return (lineToText page)

-- 1 - Load first version of the /chatton page
-- 2 - Load new version of /chatton
--     > if different, alert (Messenger, Slack, SMS??)
-- 3 - Recurse step 2 with latest /chatton
-- TODO Use something like forever instead of recursion?
checkNewKatt :: Shell ()
checkNewKatt = curlPage url >>= recCheckNewKatt

recCheckNewKatt :: Text -> Shell ()
recCheckNewKatt basePage = do
  liftIO $ print "Checking page..."
  page <- curlPage url
  when (page /= basePage) (liftIO $ print "Something changed on the page!")
  sleep (60.0 * 5.0) -- wait for 5 minutes
  recCheckNewKatt page

main :: IO ()
main = sh checkNewKatt
