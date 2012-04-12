{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
import Network.Socket
import Data.Conduit as C
import Data.Conduit.Network
import Data.Conduit.Attoparsec
import Data.Attoparsec.Char8 as Char8
import qualified Data.Attoparsec as Word8
import Data.ByteString.Char8 as BS

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Applicative

import Data.Word
import Data.String
import Data.Char hiding (isSpace, isDigit)

--Types--

type ServerName = ByteString

data IRCMsg = IRCMsg { msgPrefix  :: Maybe (Either UserInfo ServerName)
                     , msgCommand :: Either ByteString (Char, Char, Char)
                     , msgParams  :: [ByteString]
                     , msgTrail   :: ByteString
                     }
            deriving (Eq, Show, Read)


data UserInfo = UserInfo { userNick  :: ByteString
                         , userIdent :: Maybe ByteString
                         , userHost  :: Maybe ByteString
                         }
              deriving (Eq, Show, Read)



type IRCSource m = Source m IRCMsg
type IRCSink   m = Sink IRCMsg m ()

type IRCNode m = IRCSource m -> IRCSink m -> m ()


data IRCClientSettings = IRCClientSettings { ircHost :: String
                                           , ircPort :: Word16
                                           , ircNick :: ByteString
                                           , ircRealName :: ByteString
                                           , ircPass :: Maybe ByteString
                                           }
                       deriving (Eq, Show, Read)

data IRCServerSettings = IRCServerSettings { serverHost :: HostPreference
                                           , serverPort :: Word16
                                           }
                       deriving (Eq, Show)
--Pipes--
sourceIRC :: (MonadIO m, MonadThrow m) => Socket -> IRCSource m
sourceIRC s = sourceSocket s $= ircParseInput


sinkIRC :: MonadIO m => Socket -> IRCSink m
sinkIRC s = ircSerializeOutput =$ sinkSocket s


ircParseInput :: (MonadIO m, MonadThrow m) => Conduit ByteString m IRCMsg
ircParseInput = C.sequence (sinkParser ircLine)

ircSerializeOutput :: MonadIO m => Conduit IRCMsg m ByteString
ircSerializeOutput = do
  mayMsg <- await
  case mayMsg of
    Nothing  -> C.Done Nothing ()
    Just msg -> yield $ BS.concat [prefix, command, params, trail, "\r\n"]
      
      where prefix = case msgPrefix msg of
              Nothing -> ""
              Just (Right serv) -> ':' `cons` serv
              Just (Left info) -> ':' `cons` userNick info
                                  `append` maybeIdent
                                  `append` maybeHost
                where 
                  maybeIdent = maybe "" ('!' `cons`) (userIdent info)
                  maybeHost  = maybe "" ('@' `cons`) (userHost info)
      
            command = ' ' `cons` either id 
                      (\(a,b,c) -> fromString [a,b,c]) (msgCommand msg)
      
            params = ' ' `cons` intercalate " " (msgParams msg)
      
            t = msgTrail msg
            trail
              | BS.null t = ""
              | otherwise = " :" `append` msgTrail msg
                      

--todo: send pass/user/nick
runIRCClient :: (MonadIO m, MonadBaseControl IO m, MonadThrow m) => 
       IRCClientSettings -> IRCNode m -> m ()
runIRCClient IRCClientSettings{ircHost = h, ircPort = p} client = 
  runTCPClient ClientSettings{clientPort = fromIntegral p, clientHost = h}
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

prefix = spaces >> char ':' >> spaces >> eitherP userInfo serverName <* spaces
  where
    serverName = host
    userInfo = UserInfo <$> nick 
                        <*> optional (char '!' >> ident)
                        <*> optional (char '@' >> host)

command = eitherP alphaComm numComm
  where alphaComm = takeWhile1 isAlpha_ascii
        numComm   = (,,) <$> satisfy isDigit
                         <*> satisfy isDigit
                         <*> satisfy isDigit

params = spaces >> (param `sepBy` spaces) 
  where param = cons
                <$> satisfy (\c -> isNonWhite c && c /= ':')
                <*> Char8.takeWhile isNonWhite

mess = spaces >> char ':' >> Word8.takeWhile (not . isEndOfLine) <* endOfLine

ircLine = IRCMsg <$> optional prefix <*> command <*> params <*> mess

main = undefined