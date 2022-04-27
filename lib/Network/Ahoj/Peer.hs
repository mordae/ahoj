-- |
-- Module      :  Network.Ahoj.Peer
-- Copyright   :  Jan Hamal Dvořák
-- License     :  MIT
--
-- Maintainer  :  mordae@anilinux.org
-- Stability   :  unstable
-- Portability :  non-portable (ghc)
--

module Network.Ahoj.Peer
  ( join
  , ahoj
  , Network.Ahoj.Peer.send
  , Network.Ahoj.Peer.recv
  , part
  , rename
  )
where
  import Network.Ahoj.Types

  import Data.Aeson

  import Control.Monad.IO.Class
  import Data.Char
  import Data.Word
  import System.Environment

  import Network.Socket
  import Network.Socket.ByteString (sendAllTo, recv)

  import Data.ByteString (toStrict, fromStrict)

  import Data.Text (Text)
  import Data.Text qualified as Text


  everyone :: SockAddr
  everyone = SockAddrInet 1337 0xffffffff


  join :: MonadIO m => m Group
  join = liftIO do
    username <- Text.pack <$> getEnv "USER"

    sock <- socket AF_INET Datagram 0
    setSocketOption sock ReuseAddr 1
    setSocketOption sock Broadcast 1
    withFdSocket sock setCloseOnExecIfNeeded
    bind sock (SockAddrInet 1337 0x00000000)

    let group = Group { grMyself = makePresence username
                      , grSocket = sock
                      }

    return group


  ahoj :: MonadIO m => Group -> m ()
  ahoj group@Group{..} = liftIO do
    send group $ MsgPresence grMyself


  send :: MonadIO m => Group -> Message -> m ()
  send Group{..} msg = liftIO do
    let payload = toStrict (encode msg)
    sendAllTo grSocket payload everyone


  recv :: MonadIO m => Group -> m Message
  recv group@Group{..} = liftIO do
    bstr <- Network.Socket.ByteString.recv grSocket 1024
    case decode (fromStrict bstr) of
      Just msg -> return msg
      Nothing -> Network.Ahoj.Peer.recv group


  part :: MonadIO m => Group -> m ()
  part group@Group{grMyself = Presence{..}} = do
    send group $ MsgUtterance Utterance { utAuthor = prAuthor
                                        , utColor  = prColor
                                        , utFlair  = FlairStatus
                                        , utBody   = "has left."
                                        }


  rename :: Text -> Group -> Group
  rename username group = group { grMyself = makePresence username }


  makePresence :: Text -> Presence
  makePresence username = Presence { prAuthor = username
                                   , prColor = selectColor username
                                   }


  selectColor :: Text -> Word8
  selectColor username = fromIntegral (sum (map ord chars) `rem` 220 + 17)
    where chars = Text.unpack username


-- vim:set ft=haskell sw=2 ts=2 et:
