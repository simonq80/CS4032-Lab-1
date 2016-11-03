module Main where
import System.IO
import qualified Data.ByteString.Char8 as B8
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString
import Options.Applicative

main = do
	putStrLn "Server IP:"
	ip <- getLine
	putStrLn "Server Port:"
	port <- getLine
	putStrLn "Message:"
	message <- getLine
	s <- createConnection ip (read port)
	m <- sendMess s message
	putStrLn "Returned:"
	B8.putStrLn m

createConnection:: String -> Int -> IO Socket
createConnection h p = do 
	addrInfo : _ <- getAddrInfo Nothing (Just h) (Just (show p))
	s <- socket (addrFamily addrInfo) Stream defaultProtocol
	connect s (addrAddress addrInfo)
	return s


sendMess :: Socket -> String -> IO B8.ByteString
sendMess s m = do
  send s (B8.pack $ getRequest "message" m)
  recv s 2048

getRequest :: String ->String -> String
getRequest p s = "GET /echo.php?" ++ p ++ "=" ++ s ++ " HTTP/1.1\r\n\r\n"
