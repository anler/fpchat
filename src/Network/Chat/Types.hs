{-# LANGUAGE OverloadedStrings #-}
module Network.Chat.Types
  (
    initServerState
  , newClient
  , ServerState
  , Command(..)
  , User(..)
  ) where

import           Control.Concurrent
import           Control.Concurrent.MVar
import qualified Data.ByteString.Char8   as B
import qualified Data.Map                as Map
import           System.IO               (Handle)

data Command a =
  AskNick
  | SetNick a
  | Msg a
  | Names
  | Quit (Maybe a)
  | Kick a
  deriving (Show, Eq)

data ServerState = ServerState (MVar (Map.Map Nick User))

data User = User Nick Handle deriving Show

type Nick = B.ByteString

initServerState :: IO ServerState
initServerState = seq users $ ServerState <$> newMVar users
  where
    users = Map.empty

newClient :: ServerState -> Handle -> IO Nick
newClient (ServerState m) handle = do
  users <- takeMVar m
  nick <- genNick users (Map.size users)
  let newState = Map.insert nick (User nick handle) users
  putMVar m newState
  seq newState $ return nick

genNick :: Map.Map Nick User -> Int -> IO Nick
genNick users n = do
  let nick = "anon" `B.append` B.pack (show n)
  if nick `Map.member` users
    then genNick users (succ n)
    else return nick
