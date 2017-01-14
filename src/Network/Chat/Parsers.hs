module Network.Chat.Parsers where

import Text.Parsec
import Text.Parsec.ByteString
import Data.ByteString

import Network.Chat.Commands

nick :: Parser (Command String)
nick = string "nick" *> (done <|> nickName)
  where
    done = pure AskNick <* try (spaces >> eof)
    nickName = SetNick <$> (many1 space *> anythingButSpace <* eof)

msg :: Parser (Command String)
msg = string "msg" *> space *> actualMsg
  where
    actualMsg = Msg <$> anythingButEof

names :: Parser (Command String)
names = string "names" *> pure Names <* eof

quit :: Parser (Command String)
quit = string "quit" *> quitMessage <* eof
  where
    quitMessage = done <|> actualMessage
    done = try $ eof *> pure (Quit Nothing)
    actualMessage = Quit . Just <$> (space *> anythingButEof)

kick :: Parser (Command String)
kick = Kick <$> (string "kick" *> space *> anythingButEof)

anythingButSpace :: Parser String
anythingButSpace = many1 (noneOf " ")

anythingButEof :: Parser String
anythingButEof = anyChar `manyTill` try eof

