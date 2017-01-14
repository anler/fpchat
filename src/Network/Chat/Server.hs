module Network.Chat.Server where

import Network.Chat.Config

runServer :: ChatConfig -> IO ()
runServer _ = putStrLn "Hello there from someFunc"
