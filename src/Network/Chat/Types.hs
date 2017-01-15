{-# LANGUAGE OverloadedStrings #-}
module Network.Chat.Types
  (
    initServerState
  , newClient
  , removeClient
  , notify
  , handleNotifications
  , ServerState
  , Command(..)
  , Notification(..)
  , User(..)
  ) where

import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString.Char8 as B
import           Data.List             (partition)
import qualified Data.Map              as Map
import           System.IO             (Handle)

data Command a =
  AskNick
  | SetNick a
  | Msg a
  | Names
  | Quit (Maybe a)
  | Kick a
  deriving (Show, Eq)

data ServerState = ServerState (MVar (Map.Map Nick User)) (Chan (Notification String))

data User = User Nick Handle deriving Show

type Nick = B.ByteString

data Notification a =
  UserMsg Handle a
  | UserJoined Handle
  | UserRename Handle a
  | UserLeft Handle (Maybe a)
  deriving Show

initServerState :: IO ServerState
initServerState = ServerState <$> newMVar Map.empty <*> newChan

newClient :: ServerState -> Handle -> IO Nick
newClient state@(ServerState m _) handle = do
  users <- takeMVar m
  nick <- genNick users (Map.size users)
  let newUsers = Map.insert nick (User nick handle) users
  putMVar m newUsers
  notify state (UserJoined handle)
  seq newUsers $ return nick

removeClient :: ServerState -> Handle -> IO ()
removeClient (ServerState m _) handle = do
  users <- takeMVar m
  let (nick, _) = partitionByHandle handle (Map.elems users)
      newUsers = Map.delete nick users
  putMVar m newUsers
  seq newUsers $ return ()

genNick :: Map.Map Nick User -> Int -> IO Nick
genNick users n = do
  let nick = "anon" `B.append` B.pack (show n)
  if nick `Map.member` users
    then genNick users (succ n)
    else return nick

notify :: ServerState -> Notification String -> IO ()
notify (ServerState _ ns) n = writeChan ns n

handleNotifications :: ServerState -> IO ()
handleNotifications (ServerState m ns) = loop
  where
    loop = do
      users <- readMVar m
      notification <- readChan ns
      _ <- forkIO $ handleNotification users notification
      loop

handleNotification :: Map.Map Nick User -> Notification String -> IO ()
handleNotification users (UserLeft handle maybemsg) = do
  let (nick, notifyHandlers) = partitionByHandle handle (Map.elems users)
  forM_ notifyHandlers $ \notifyHandle -> do
    let msg = maybe "" (" " ++) maybemsg
    B.hPutStrLn notifyHandle ("LEFT: " `B.append` nick `B.append` B.pack msg)

userHandle :: User -> Handle
userHandle (User _ h) = h

userNick :: User -> Nick
userNick (User n _) = n

partitionByHandle :: Handle -> [User] -> (Nick, [Handle])
partitionByHandle handle users =
  let ([user], notifyUsers) = partition ((== handle) . userHandle) users
  in
    (userNick user, userHandle <$> notifyUsers)
