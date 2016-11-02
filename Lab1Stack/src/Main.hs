module Main where
import System.IO
import qualified Data.ByteString.Char8 as B8
import Network.HTTP.Base (urlEncode)
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString
import Options.Applicative

main = do
	s <- createConnection "localhost" 8000
	m <- sendMess s "testtest"
	B8.putStrLn m

createConnection:: String -> Int -> IO Socket
createConnection h p = do 
	addrInfo : _ <- getAddrInfo Nothing (Just h) (Just (show p))
	s <- socket (addrFamily addrInfo) Stream defaultProtocol
	connect s (addrAddress addrInfo)
	return s


sendMess :: Socket -> String -> IO B8.ByteString
sendMess s m = do
  send s (B8.pack $ getRequest m)
  recv s 2048

getRequest :: String -> String
getRequest s = "GET /echo.php?message=" ++ s ++ " HTTP/1.1\r\n\r\n"
