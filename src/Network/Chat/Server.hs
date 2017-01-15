{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Chat.Server where

import           Control.Concurrent    (forkIO)
import           Control.Exception     (finally)
import           Control.Monad         (forever)
import qualified Data.ByteString.Char8 as B
import           Network               (PortID (..), accept, listenOn,
                                        withSocketsDo)
import           Prelude               hiding (putStrLn, (++))
import           System.IO             (BufferMode (..), Handle, hClose,
                                        hSetBuffering, hSetNewlineMode, stdout,
                                        universalNewlineMode)

import           Network.Chat.Config
import           Network.Chat.Parsers  (parseCmd)
import           Network.Chat.Types

runServer :: ChatConfig -> IO ()
runServer ChatConfig { port } = withSocketsDo $ do
  hSetBuffering stdout LineBuffering
  state <- initServerState
  socket <- listenOn (asPort port)
  putStrLn $ "Server running on port " ++ showB port
  forever $ do
    (handle, host, clientPort) <- accept socket
    let orig = B.pack host ++ ": " ++ showB clientPort
    hSetNewlineMode handle universalNewlineMode
    hSetBuffering handle LineBuffering
    putStrLn $ "Accepted connection from " ++ orig
    forkIO $ handleClient state handle `finally` do
      putStrLn $ "Closing connection of " ++ orig
      hClose handle
  where
    asPort = PortNumber . fromIntegral

handleClient :: ServerState -> Handle -> IO ()
handleClient state handle = do
  nick <- genClientNick state
  B.hPutStrLn handle ("Welcome to fpchat! You're identified with nick: " `B.append` nick)
  loop
  where
    loop = do
      cmd <- B.hGetLine handle
      putStrLn $ "Received command: " ++ cmd
      case parseCmd cmd of
        Right cmd -> handleCmd cmd
        Left _    -> do
          B.hPutStrLn handle ("400 Unrecognised command")
          loop

    handleCmd cmd = do
      case cmd of
        Quit msg -> return ()
        _        -> loop

putStrLn :: B.ByteString -> IO ()
putStrLn s = B.hPutStrLn stdout s

(++) = B.append

showB :: Show a => a -> B.ByteString
showB = B.pack . show
