{-# LANGUAGE OverloadedStrings #-}
module Network.Chat.Types
  (
    initServerState
  , genClientNick
  , ServerState
  , Command(..)
  , User(..)
  ) where

import           Control.Concurrent
import qualified Data.ByteString    as B
import qualified Data.Set           as Set

data Command a =
  AskNick
  | SetNick a
  | Msg a
  | Names
  | Quit (Maybe a)
  | Kick a
  deriving (Show, Eq)

data ServerState = ServerState (MVar (Set.Set User))

data User = User B.ByteString deriving Show

initServerState :: IO ServerState
initServerState = seq users $ ServerState <$> newMVar users
  where
    users = Set.empty

genClientNick :: ServerState -> IO B.ByteString
genClientNick _ = return "anon1"

