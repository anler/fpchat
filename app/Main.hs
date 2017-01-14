module Main where

import Network.Chat
import Options.Applicative

chatOptions :: Parser ChatConfig
chatOptions = ChatConfig
  <$> option auto
  (long "port"
    <> short 'p'
    <> metavar "PORT"
    <> showDefault
    <> value 3030
    <> help "Port number in which the chat runs.")

main :: IO ()
main = execParser opts >>= runServer
  where
    opts = info (helper <*> chatOptions)
      (fullDesc
      <> progDesc "Run the chat server."
      <> header "fpchat - Haskell version of NacionLumpen's chat exercise.")
