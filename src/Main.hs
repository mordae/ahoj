-- |
-- Module      :  Main
-- Copyright   :  Jan Hamal Dvořák
-- License     :  MIT
--
-- Maintainer  :  mordae@anilinux.org
-- Stability   :  unstable
-- Portability :  non-portable (ghc)
--

module Main
  ( main
  )
where
  import Network.Ahoj.Types
  import Network.Ahoj.Peer qualified as Ahoj

  import Control.Monad
  import Control.Concurrent
  import Control.Exception
  import Control.Monad.IO.Class (liftIO)

  import System.Exit
  import System.Console.Haskeline

  import Data.Word
  import Data.Text qualified as Text


  main :: IO ()
  main = bracket Ahoj.join Ahoj.part chat


  settings :: Settings IO
  settings = Settings { complete = noCompletion
                      , historyFile = Nothing
                      , autoAddHistory = False
                      }


  chat :: Group -> IO ()
  chat group = do
    runInputT settings do
      Ahoj.ahoj group

      safePrint <- getExternalPrint
      void $ liftIO $ forkIO $ forever do
        msg <- Ahoj.recv group

        case msg of
          MsgPresence Presence{..} -> do
            safePrint $
              mconcat [ setColor prColor
                      , Text.unpack prAuthor
                      , " has joined."
                      , unsetColor
                      , "\n"
                      ]

          MsgUtterance Utterance{..} -> do
            case utFlair of
              FlairChat -> do
                safePrint $
                  mconcat [ setColor utColor
                          , Text.unpack utAuthor
                          , ">"
                          , unsetColor
                          , " "
                          , Text.unpack utBody
                          , "\n"
                          ]

              FlairStatus -> do
                safePrint $
                  mconcat [ setColor utColor
                          , Text.unpack utAuthor
                          , " "
                          , Text.unpack utBody
                          , unsetColor
                          , "\n"
                          ]

      forever do
        maybeLine <- getInputLine (prompt group)

        case maybeLine of
          Nothing -> liftIO exitSuccess
          Just "" -> return ()
          Just line -> do
            let Group{grMyself = Presence{..}} = group
                utAuthor = prAuthor
                utColor  = prColor
                utBody   = Text.pack line
                utFlair  = FlairChat
            Ahoj.send group $ MsgUtterance Utterance{..}


  prompt :: Group -> String
  prompt Group{grMyself = Presence{..}} =
    mconcat [ "\ESC[1m"
            , Text.unpack prAuthor
            , ":"
            , unsetColor
            , " "
            ]


  setColor :: Word8 -> String
  setColor color = "\ESC[38;5;" <> show color <> "m"


  unsetColor :: String
  unsetColor = "\ESC[0m"



-- vim:set ft=haskell sw=2 ts=2 et:
