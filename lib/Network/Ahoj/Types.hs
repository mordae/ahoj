-- |
-- Module      :  Network.Ahoj.Types
-- Copyright   :  Jan Hamal Dvořák
-- License     :  MIT
--
-- Maintainer  :  mordae@anilinux.org
-- Stability   :  unstable
-- Portability :  non-portable (ghc)
--

module Network.Ahoj.Types
  ( Group(..)
  , Message(..)
  , Presence(..)
  , Utterance(..)
  , Flair(..)
  )
where
  import Data.Text (Text)

  import Data.Char
  import Data.Word (Word8)
  import GHC.Generics
  import Network.Socket (Socket)

  import Data.Aeson
  import Data.Aeson.Types


  data Group
    = Group
      { grMyself       :: Presence
      , grSocket       :: Socket
      }
    deriving (Eq, Show)


  data Message
    = MsgUtterance Utterance
    | MsgPresence Presence
    deriving (Eq, Show, Generic)

  instance FromJSON Message where
    parseJSON = genericParseJSON jsonOptions

  instance ToJSON Message where
    toJSON = genericToJSON jsonOptions


  data Utterance
    = Utterance
      { utAuthor       :: Text
      , utBody         :: Text
      , utFlair        :: Flair
      , utColor        :: Word8
      }
    deriving (Eq, Show, Generic)

  instance FromJSON Utterance where
    parseJSON = genericParseJSON jsonOptions

  instance ToJSON Utterance where
    toJSON = genericToJSON jsonOptions


  data Presence
    = Presence
      { prAuthor       :: Text
      , prColor        :: Word8
      }
    deriving (Eq, Show, Generic)

  instance ToJSON Presence where
    toJSON = genericToJSON jsonOptions

  instance FromJSON Presence where
    parseJSON = genericParseJSON jsonOptions


  data Flair
    = FlairChat
    | FlairStatus
    deriving (Eq, Show)

  instance FromJSON Flair where
    parseJSON (String "chat")   = pure FlairChat
    parseJSON (String "status") = pure FlairStatus
    parseJSON invalid = typeMismatch "Flair" invalid

  instance ToJSON Flair where
    toJSON FlairChat   = String "chat"
    toJSON FlairStatus = String "status"


  jsonOptions :: Options
  jsonOptions = defaultOptions { fieldLabelModifier = map toLower . drop 2
                               , sumEncoding = UntaggedValue
                               }


-- vim:set ft=haskell sw=2 ts=2 et:
