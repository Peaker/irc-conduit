{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Network.IRC.Conduit.Raw
       ( -- |IRC messages
         ServerName, IRCMsg, UserInfo
         -- |IRC sources and sinks
       ,  IRCSource, IRCSink, IRCNode
       , sourceIRC, sinkIRC
         -- |IRC conduits
       , ircParseInput, ircSerializeOutput
         -- |IRC clients
        ,IRCClientSettings, runIRCClient
        ) where
import Network.Socket
import Data.Conduit as C
import Data.Conduit.List as CL
import Data.Conduit.Network
import Data.Conduit.Attoparsec
import Data.Attoparsec.Char8 as Char8
import qualified Data.Attoparsec as Word8
import Data.ByteString.Char8 as BS 
  (cons, append, intercalate, ByteString, null, concat, pack, unpack)

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Applicative

import Data.Maybe
import Data.Function
import Data.Word
import Data.String
import Data.Char hiding (isSpace, isDigit)
import Prelude hiding (takeWhile)

--Types--

type ServerName = ByteString

data IRCMsg = IRCMsg { msgPrefix  :: Maybe (Either UserInfo ServerName)
                     , msgCmd     :: Either ByteString (Char, Char, Char)
                     , msgParams  :: [ByteString]
                     , msgTrail   :: ByteString
                     }
            deriving (Eq)


data UserInfo = UserInfo { userNick  :: ByteString
                         , userName  :: Maybe ByteString
                         , userHost  :: Maybe ByteString
                         }
              deriving (Eq, Show, Read)



type IRCSource m = Source m IRCMsg
type IRCSink   m = Sink IRCMsg m ()

type IRCNode m = IRCSource m -> IRCSink m -> m ()


data IRCClientSettings = IRCClientSettings { ircHost :: String
                                           , ircPort :: Word16
                                           , ircNick :: ByteString
                                           , ircUser :: ByteString
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
ircSerializeOutput = CL.map fromIRCMsg

toIRCMsg :: ByteString -> Result IRCMsg
toIRCMsg = parse ircLine

fromIRCMsg :: IRCMsg -> ByteString
fromIRCMsg msg = BS.concat $ [prefix, command, params, trail, "\r\n"]
  where prefix = case msgPrefix msg of
          Nothing -> ""
          Just (Right serv) -> ':' `cons` serv `append` " "
          Just (Left info) -> ':' `cons` userNick info
                              `append` maybeUser
                              `append` maybeHost
            where 
              maybeUser  = maybe ""  ('!' `cons`) (userName info)
              maybeHost  = maybe " " ('@' `cons`) (userHost info)
      
        command = either id 
                  (\(a,b,c) -> fromString [a,b,c]) (msgCmd msg)            
          
        params = ' ' `cons` intercalate " " (msgParams msg)
      
        t = msgTrail msg
        trail
          | BS.null t = ""
          | otherwise = " :" `append` msgTrail msg

instance Show IRCMsg where
  show = BS.unpack . fromIRCMsg
  
instance Read IRCMsg where
  readsPrec _ str = 
    case toIRCMsg (BS.pack str) of
      Word8.Done leftover msg -> [(msg, BS.unpack leftover)]
      _ -> []

      
printOutput :: MonadIO m => Conduit IRCMsg m IRCMsg 
printOutput = CL.mapM $ \msg -> do
                                liftIO $ print msg
                                return msg
                      

--todo: send pass/user/nick
runIRCClient :: (MonadIO m, MonadBaseControl IO m, MonadThrow m) => 
       IRCClientSettings -> IRCNode m -> m ()
runIRCClient s client = 
  runTCPClient ClientSettings{clientPort = fromIntegral (ircPort s), 
                              clientHost = ircHost s}
    $ \src snk -> 
      let inp = src $= ircParseInput
          out = ircSerializeOutput =$ snk  
      in client (inp) (connect =$ out)
  where
    connect = do
      case ircPass s of
        Just pass -> yield $ msg "PASS" [pass] ""
        Nothing   -> return ()
      yield $ msg "USER" [ircUser s, "*", "*"] (ircRealName s)
      yield $ msg "NICK" [ircNick s] ""
      printOutput
      where
        msg cmd params trail = IRCMsg Nothing (Left cmd) params trail

main = do
  runIRCClient IRCClientSettings {ircHost = ""
                                 , ircPort = 6667
                                 , ircNick = "ero"
                                 , ircUser = "bot"
                                 , ircRealName = "."
                                 , ircPass = Nothing 
                                 }
    $ \src snk -> ((src) $$ (test =$ snk) :: IO ())
    where
      test = do
        --fix $ \p -> await >>= maybe (return ()) (\i -> liftIO (print i) >> p)
        yield $ msg "JOIN" ["#haskell"] ""
        yield $ msg "PRIVMSG" ["#haskell"] "Hello, World!"
        yield $ msg "QUIT" [] ""
        where
          msg cmd params trail = IRCMsg Nothing (Left cmd) params trail
          
          
--Parsers--
spaces = takeWhile isSpace   <?> "optional whitespace"
spaces1 = takeWhile1 isSpace <?> "whitespace"

isAlphanum c = isAlpha_ascii c || isDigit c

isNonWhite c = c /= ' ' && c /= '\r' && c /= '\n' && c /= '\0'

isChanPrefix c = c == '#' || c  == '$'
isChanChar c = isNonWhite c && c /= ','

chan = cons 
       <$> (satisfy isChanPrefix  <?> "channel prefix")
       <*> (takeWhile1 isChanChar <?> "channel name")

isNickChar c = isNonWhite c && c /= '!' && c /= '@'

nick = takeWhile1 isNickChar <?> "nick"

isUserChar c = isNonWhite c && c /= '@'

user = takeWhile1 isUserChar <?> "username"

host = takeWhile1 isNonWhite <?> "hostname"

prefix = spaces >> char ':' >> spaces >> eitherP userInfo serverName
         <?> "prefix"
  where
    serverName = host
    userInfo = UserInfo <$> nick 
                        <*> optional (char '!' >> user)
                        <*> optional (char '@' >> host)

command = spaces1 >> eitherP alphaComm numComm
  where alphaComm = takeWhile1 isAlpha_ascii <?> "command name"
        numComm   = (,,) <$> satisfy isDigit
                         <*> satisfy isDigit
                         <*> satisfy isDigit
                         <?> "numeric command"

params = spaces1 >> param `sepBy` spaces <?> "params list"
  where param = cons
                <$> satisfy (\c -> isNonWhite c && c /= ':')
                <*> Char8.takeWhile isNonWhite

mess = spaces1 >> char ':' >> Word8.takeWhile (not . isEndOfLine) <* endOfLine
       <?> "message body"

ircLine = IRCMsg <$> optional prefix <*> command <*> params <*> mess
          <?> "IRC line"
