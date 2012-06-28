import Data.Bits
import Network.Socket
import Network.BSD
import Data.List
import SyslogTypes

data SyslogHandle =
	SyslogHandle { slSocket :: Socket,
				   slProgram :: String,
				   slAddress :: SockAddr }

openlog :: Hostname -- ^ Remote hostname, or localhost
		-> String   -- ^ Port number or name
		-> String   -- ^ Name to log under
		-> IO SyslogHandle -- ^ Handle to use for logging

openlog hostname port progname =
	do
		addrinfos <- getAddrInfo Nothing (Just hostname) (Just port) -- IO assignment
		let serveraddr = head addrinfos -- we voeren getAddrInfo uit om de server address te krijgen
		
		sock <- socket (addrFamily serveraddr) Datagram defaultProtocol -- we maken een socket aan met het juiste protocol
		return $ SyslogHandle sock progname (addrAddress serveraddr) -- we returnen een data structure met de socket, progname en het adres

syslog :: SyslogHandle -> Facility -> Priority -> String -> IO () -- IO () is als een void procedure
syslog syslogh fac pri msg = -- hij neemt dus een facility en een priority, maakt daar een code van en stuurt die met een message naar de socket
	sendstr sendmsg
	where
		code = makeCode fac pri
		sendmsg = "<" ++ show code ++ ">" ++ (slProgram syslogh) ++ ": "  ++ msg -- slProgram handle haalt de slProgram uit de datastructure
		
		sendstr :: String -> IO ()
		sendstr [] = return ()
		sendstr omsg = do
						sent <- sendTo (slSocket syslogh) omsg (slAddress syslogh) -- stuur op de socket, het bericht naar het adres en sla wat verstuurd is op
						sendstr (genericDrop sent omsg) -- recursieve aanroep, om te sturen wat nog niet verstuurd is

closelog :: SyslogHandle -> IO ()
closelog syslogh = sClose (slSocket syslogh)

makeCode :: Facility -> Priority -> Int
makeCode fac pri =
	let faccode = codeOfFac fac
		pricode = fromEnum pri
		in
			(faccode `shiftL` 3) .|. pricode
