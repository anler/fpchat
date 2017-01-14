module Network.Chat.Commands where

data Command a =
  AskNick
  | SetNick a
  | Msg a
  | Names
  | Quit (Maybe a)
  | Kick a
  deriving (Show, Eq)
