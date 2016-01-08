-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE TypeOperators #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types where

-- http://artyom.me/aeson

import Control.Monad
import Data.Aeson
import Data.Aeson.TH
import Data.Char
import Text.Show
-- import Data.String.Conversions (cs)

import Telegram.Aux

import qualified Data.Text as T

newtype TgTrue = TgTrue { _tgTrue :: Bool } deriving (Eq, Show)

instance FromJSON TgTrue where
  parseJSON (Bool True) = pure $ TgTrue True
  parseJSON _ = pure $ TgTrue False

instance ToJSON TgTrue where
  toJSON (TgTrue True) = Bool True
  toJSON _ = Null

data Update = Update {
  _updateUpdateId :: TID
  , _updateMessage :: Maybe Message
  -- , _updateInlineQuery :: Maybe InlineQuery
  -- , _updateChoosenInlineResult :: Maybe ChoosenInlineResult
} deriving (Eq, Show)

data Message = Message {
  _messageMessageId :: TID
  , _messageFrom :: Maybe User
  , _messageDate :: TID
  -- , _messageChat :: Chat
  , _messageForwardFrom :: Maybe User
  , _messageForwardDate :: Maybe TID
  , _messageReplyToMessage :: Maybe Message
  , _messageText :: Maybe T.Text
  -- , _messageAudio :: Maybe Audio
  -- , _messageDocument :: Maybe Document
  -- , _messagePhoto :: Maybe [PhotoSize]
  -- , _messageSticker :: Maybe Sticker
  -- , _messageVideo :: Maybe Video
  -- , _messageVoice :: Maybe Voice
  , _messageCaption :: Maybe T.Text
  -- , _messageLocation :: Maybe Location
  , _messageNewChatParticipant :: Maybe User
  , _messageLeftChatParticipant :: Maybe User
  , _messageNewChatTitle :: Maybe T.Text
  -- , _messageNewChatPhoto :: Maybe [PhotoSize]
  , _messageDeleteChatPhoto :: Maybe TgTrue
  , _messageGroupChatCreated :: Maybe TgTrue
  , _messageSupergroupChatCreated :: Maybe TgTrue
  , _messageChannelChatCreated :: Maybe TgTrue
  , _messageMigrateToChatId :: Maybe TID
  , _messageMigrateFromChatId :: Maybe TID
} deriving (Eq, Show)

data User = User {
  _userId :: TID
  , _userFirstName :: T.Text
  , _userLastName :: Maybe T.Text
  , _userUsername :: Maybe T.Text
  }  deriving (Eq, Show)

$(deriveJSON tgJSONOptions {fieldLabelModifier = snakeDrop 5} ''User)
$(deriveJSON tgJSONOptions {fieldLabelModifier = snakeDrop 7} ''Update)
$(deriveJSON tgJSONOptions {fieldLabelModifier = snakeDrop 8} ''Message)

-- instance FromJSON User where
--   parseJSON = withObject "user" $ \o -> do
--     id <- o.: "id"
--     first_name <- o.: "first_name"
--     last_name <-  optional (o .: "last_name")
--     username <-  optional (o .: "username")
--     return User{..}
