{-# LANGUAGE NamedFieldPuns #-}
module M2Handler where
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS
import Data.List
  (intercalate)
import Text.JSON
  (JSObject, JSString, Result(..), decodeStrict, fromJSObject, fromJSString)
import qualified Text.ParserCombinators.Parsec as P
import qualified System.ZMQ as Z

data M2Context =
  M2Context { m2PullSocket :: Z.Socket Z.Pull
            , m2PubSocket :: Z.Socket Z.Pub
            }

m2Init :: String -> String -> IO M2Context
m2Init pullSpec pubSpec = do
  zctx <- Z.init 1
  zpull <- Z.socket zctx Z.Pull
  zpub <- Z.socket zctx Z.Pub
  Z.connect zpull pullSpec
  Z.connect zpub pubSpec
  return (M2Context zpull zpub)

data UUID =
  UUID String

instance Show UUID where
  show (UUID str) = str

data M2Request =
  M2Request { reqServerId :: UUID
            , reqClientId :: Int
            , reqPath :: String
            , reqHeaders :: [(String,String)]
            , reqBody :: String
            }
  deriving (Show)

data M2Response =
  M2Response { resServerId :: UUID
             , resClientIds :: [Int]
             , resCode :: Int
             , resHeaders :: [(String,String)]
             , resBody :: String
             }                  
  deriving (Show)

m2recv :: M2Context -> IO M2Request
m2recv M2Context{m2PullSocket} =
  fmap (parseM2Request . BS.unpack) (Z.receive m2PullSocket [])

m2send :: M2Context -> M2Response -> IO ()
m2send M2Context{m2PubSocket} res =
  Z.send' m2PubSocket (packResponse res) []

parseM2Request :: String ->  M2Request
parseM2Request reqstr =
  case (P.parse parseRequest "M2Handler.parseM2Request" reqstr) of
    Left err -> error (show err)
    Right m2r -> m2r

parseNumber :: P.Parser Int
parseNumber = fmap read (P.many1 P.digit)

parseNetstring :: P.Parser String
parseNetstring = do
  len  <- parseNumber
  _    <- P.char ':'
  body <- P.count len P.anyChar
  _    <- P.char ','
  return body

parseHeaders :: P.Parser [(String,String)]
parseHeaders = do
  raw <- parseNetstring
  let (Ok obj) = decodeStrict raw :: Result (JSObject JSString)
  return $ fmap (\(k,v)->(k,fromJSString v)) (fromJSObject obj)

parseRequest :: P.Parser M2Request
parseRequest = do
  sid     <- P.many1 (P.noneOf " ")
  _       <- P.space
  cid     <- parseNumber
  _       <- P.space
  path    <- P.many1 (P.noneOf " ")
  _       <- P.space
  headers <- parseHeaders
  body    <- parseNetstring
  return (M2Request (UUID sid) cid path headers body)

packResponse :: M2Response -> BSL.ByteString
packResponse M2Response{resServerId,resClientIds,resCode,resHeaders,resBody} = BSL.pack (show resServerId ++ " " ++ show clients_len ++ ":" ++ clients ++ ", " ++ formatResponseBody resCode resHeaders resBody)
  where clients = intercalate " " (map show resClientIds)
        clients_len = (length clients)

getCodeDescription :: Int -> String
getCodeDescription 200 = "OK"
getCodeDescription 404 = "Not Found"
getCodeDescription _   = error "Invalid Code"

formatResponseBody :: Int -> [(String,String)] -> String -> String
formatResponseBody code headers body =
  "HTTP/1.1 " ++ show code ++ " " ++ getCodeDescription code ++ "\r\n" ++ concatMap (\(k,v) -> k ++ ": " ++ v ++ "\r\n") headers ++ "\r\n" ++ body

