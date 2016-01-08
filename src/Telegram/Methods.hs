{-# LANGUAGE DataKinds #-}
--{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--{-# LANGUAGE StandaloneDeriving #-}
--{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Telegram.Methods where

import Control.Monad
import Data.Aeson
import Data.Aeson.TH
import Data.Char
import Text.Show

import Telegram.Aux

import Data.Text (Text)

type ChatId = OneOf '[TID, Text]

data SendMessageData = SendMessageData {
  _sendParseMode :: Maybe Text -- Better type
  , _sendDisableWebPagePreview :: Maybe Bool
  , _sendReplyToMessageId :: Maybe TID
    -- , _sendReplyMarkup :: Maybe TODO
  , _sendChatId :: ChatId
  , _sendText :: Text
} deriving (Eq, Show)

sendMessageData :: ChatId -> Text -> SendMessageData
sendMessageData = SendMessageData Nothing Nothing Nothing -- Nothing

$(deriveJSON tgJSONOptions {fieldLabelModifier = snakeDrop 5} ''SendMessageData)
