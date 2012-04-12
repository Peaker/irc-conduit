{-# LANGUAGE FlexibleContexts #-}
import Network.Socket
import Data.Conduit as C
import Data.Conduit.Network
import Data.Conduit.Attoparsec
import Data.Attoparsec.Char8 as Char8
import qualified Data.Attoparsec as Word8
import Data.ByteString.Char8 (ByteString, cons)

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Applicative

--Types--

type ServerName = ByteString

data IRCSettings = IRCSettings { ircHost :: String
                               , ircPort :: Int
                               }
                 deriving (Eq, Show, Read)

data UserInfo = UserInfo { userNick  :: ByteString
                         , userIdent :: Maybe ByteString
                         , userHost  :: Maybe ByteString
                         }
              deriving (Eq, Show, Read)


data IRCInput = IRCInput { inpPrefix  :: Maybe (Either UserInfo ServerName)
                         , inpCommand :: Either ByteString Int
                         , inpParams  :: [ByteString]
                         , inpMsg     :: ByteString
                         }
              deriving (Eq, Show, Read)

data IRCOutput
--     deriving (Eq, Show, Read)

type IRCClient m = Source m IRCInput -> Sink IRCOutput m () -> m ()


--Pipes--
sourceIRC :: (MonadIO m, MonadThrow m) => Socket -> Source m IRCInput
sourceIRC s = sourceSocket s $= ircParseInput


sinkIRC :: MonadIO m => Socket -> Sink IRCOutput m () 
sinkIRC s = ircSerializeOutput =$ sinkSocket s


ircParseInput :: (MonadIO m, MonadThrow m) => Conduit ByteString m IRCInput
ircParseInput = C.sequence (sinkParser ircLine)

ircSerializeOutput :: MonadIO m => Conduit IRCOutput m ByteString
ircSerializeOutput = undefined

irc :: (MonadIO m, MonadBaseControl IO m, MonadThrow m) => 
       IRCSettings -> IRCClient m -> m ()
irc IRCSettings{ircHost = h, ircPort = p} client = 
  runTCPClient ClientSettings{clientPort = p, clientHost = h}
  $ \src snk -> 
    client (src $= ircParseInput) (ircSerializeOutput =$ snk)



--Parsers--
spaces = takeWhile1 isSpace

isAlphanum c = isAlpha_ascii c || isDigit c

isNonWhite c = c /= ' ' && c /= '\r' && c /= '\n' && c /= '\0'

isChanPrefix c = c == '#' || c  == '$'
isChanChar c = isNonWhite c && c /= ','

chan = cons 
       <$> satisfy isChanPrefix 
       <*> takeWhile1 isChanChar

isNickChar c = isNonWhite c && c /= '!' && c /= '@'

nick = takeWhile1 isNickChar

isIdentChar c = isNonWhite c && c /= '@'

ident = takeWhile1 isIdentChar

host = takeWhile1 isNonWhite

prefix = char ':' >> eitherP userInfo serverName <* spaces
  where
    serverName = host
    userInfo = UserInfo <$> nick 
                        <*> optional (char '!' >> ident)
                        <*> optional (char '@' >> host)

command = eitherP alphaComm numComm
  where alphaComm = takeWhile1 isAlpha_ascii
        numComm   = read <$> count 3 (satisfy isDigit)


params = spaces >> (param `sepBy` spaces) 
  where param = cons
                <$> satisfy (\c -> isNonWhite c && c /= ':')
                <*> Char8.takeWhile isNonWhite

mess = spaces >> char ':' >> Word8.takeWhile (not . isEndOfLine) <* endOfLine

ircLine = IRCInput <$> optional prefix <*> command <*> params <*> mess

main = undefined