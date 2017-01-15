{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Chat.Server where

import           Control.Concurrent    (forkIO)
import           Control.Exception     (finally)
import           Control.Monad         (forever)
import qualified Data.ByteString.Char8 as B
import           Network               (PortID (..), accept, listenOn,
                                        withSocketsDo)
import           System.IO             (BufferMode (..), Handle, hClose,
                                        hSetBuffering, hSetNewlineMode, stdout,
                                        universalNewlineMode)

import           Network.Chat.Config
import           Network.Chat.Types

runServer :: ChatConfig -> IO ()
runServer ChatConfig { port } = withSocketsDo $ do
  hSetBuffering stdout LineBuffering
  state <- initServerState
  socket <- listenOn (asPort port)
  putStrLn $ "Server running on port " ++ show port
  forever $ do
    (handle, host, clientPort) <- accept socket
    hSetNewlineMode handle universalNewlineMode
    hSetBuffering handle LineBuffering
    putStrLn $ "Accepted connection from " ++ host ++ ": " ++ show clientPort
    forkIO $ handleClient state handle `finally` hClose handle
  where
    asPort = PortNumber . fromIntegral

handleClient :: ServerState -> Handle -> IO ()
handleClient state handle = do
  nick <- genClientNick state
  B.hPutStrLn handle ("Welcome to fpchat! You're identified with nick: " `B.append` nick)
