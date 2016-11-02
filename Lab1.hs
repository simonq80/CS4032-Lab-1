import System.IO
import qualified Data.ByteString.Char8 as B8
import Network.HTTP.Base (urlEncode)
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString
import Options.Applicative

main = createConnection "scss.tcd.ie" 80

createConnection:: String -> Int -> IO Socket
createConnection h p = do 
	addrInfo <- getAddrInfo Nothing (Just h) (Just (show p))
	s <- socket (addrFamily addrInfo) Stream defaultProtocol
	connect s (addrAddress addrInfo)
	return s


sendMess :: Socket -> String -> IO B8.ByteString
sendMess s m = do
  send s (B8.pack $ httpString m)
  recv s 2048