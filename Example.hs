module Main where
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import M2Handler
import System.Locale (defaultTimeLocale)

pull_spec, pub_spec :: String
pull_spec = "tcp://127.0.0.1:9000"
pub_spec = "tcp://127.0.0.1:9001"

getHttpDate :: IO String
getHttpDate = fmap (formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S %Z") getCurrentTime

main :: IO ()
main = do
  loop =<< m2Init pull_spec pub_spec
  where loop m2ctx = do
          req <- m2recv m2ctx
          print req
          respdate <- getHttpDate
          let respbody = "hello, world!"
              respheaders = [ ("date", respdate)
                            , ("content-type", "text/plain; charset=UTF-8")
                            , ("content-length", show (length respbody))
                            , ("server", "Mongrel2")
                            ]
              resp = M2Response (reqServerId req) [(reqClientId req)] 200 respheaders respbody
          print resp
          m2send m2ctx resp
          loop m2ctx


