module SimpleHttp ( simpleHttpDecode ) where

-- Library Imports
import Network.Socket ( Socket )
import Network.Socket.ByteString ( recv, sendAll )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC ( pack, unpack )
import Control.Monad ( when )

bufferSize :: Int
bufferSize = 1024

maxHeaderLength :: Int
maxHeaderLength = 16384

------- Configuration ---------
supportedMethods :: [String]
supportedMethods = ["GET", "HEAD"]

--- HTTP Error Status Codes ---

-- Malformed request
send400 :: Socket -> IO ()
send400 sock = sendAll sock $ BSC.pack "HTTP/1.1 400 Bad Request\r\n\r\n"

-- File is not readable?
send403 :: Socket -> IO ()
send403 sock = sendAll sock $ BSC.pack "HTTP/1.1 403 Forbidden\r\n\r\n"

-- File not found (or outside the scope of the server directory)
send404 :: Socket -> IO ()
send404 sock = sendAll sock $ BSC.pack "HTTP/1.1 404 Not Found\r\n\r\n"

-- Invalid method
send501 :: Socket -> IO ()
send501 sock = sendAll sock $ BSC.pack "HTTP/1.1 501 Not Implemented\r\n\r\n"

-- Invalid version
send505 :: Socket -> IO ()
send505 sock = sendAll sock $ BSC.pack "HTTP/1.1 505 HTTP Version Not Supported\r\n\r\n"


---------- Helpers ------------

-- Stops reading once it sees \r\n\r\n, or EOF
readRequest :: Socket -> IO BS.ByteString
readRequest sock = getHeaders BS.empty 0
    where
        getHeaders :: BS.ByteString -> Int -> IO BS.ByteString
        getHeaders buff bytesRead = do
            chunk <- recv sock bufferSize

            let newBytesRead = bytesRead + BS.length chunk
            if newBytesRead > maxHeaderLength then
                -- Force a 400 bad request if length exceeded
                return BS.empty
            else if BS.null chunk then
                -- EOF, return what we have
                return buff
            else do
                -- Append new chunk to buffer
                let newBuffer = buff `BS.append` chunk
                
                -- Check for \r\n\r\n
                if hasHeaderEnd newBuffer then
                    -- Return the final buffer, not caring if more data is after header
                    return newBuffer    
                else
                    -- Continue reading the header
                    getHeaders newBuffer newBytesRead

        -- Function to check if \r\n\r\n is in the buffer
        hasHeaderEnd :: BS.ByteString -> Bool
        hasHeaderEnd buf = BS.isInfixOf (BSC.pack "\r\n\r\n") buf

-- Tail but it can handle null
tailSafe :: [a] -> [a]
tailSafe [] = []
tailSafe xs = tail xs

-- Breaks up the request line by spaces into a triple
unpackReqLine :: String -> (String, String, String)
unpackReqLine str = (fst split1, fst split2, tailSafe $ snd split2)
    where 
        split1 = break (' '==) str
        split2 = break (' '==) (tailSafe $ snd split1)

-- Checks if the string begins with a /
checkHeadSlash :: String -> Bool
checkHeadSlash str = case str of
    (c:cs)  -> c == '/'
    []      -> False

-- Takes any number of consecutive slashes and replaces them with a single slash
condenseSlashes :: String -> String
condenseSlashes str = helper str []
    where
        helper str' res = case str' of
            ('/':'/':cs)    -> helper ('/':cs)   res
            (c:cs)          -> helper cs         (c:res)
            []              -> reverse res
    
        
-- Ensures no "naughtiness" in the unix filepath 
-- Prevents backtracking, and other inappropriate actions that might compromise security
-- Placeholder implementation
sanitizePath :: String -> String
sanitizePath path = path

---------- Exported -----------

-- Returns (method, filepath)
-- Empty method string signifies an error has already been sent to the client
simpleHttpDecode :: Socket -> IO (String, String)
simpleHttpDecode sock = do

    -- Recieve the request
    request <- readRequest sock
    putStrLn ("FULL REQUEST:\n" ++ show request)
    putStrLn "----------------------------------"
    
    -- Decode the request line
    let reqLine = BSC.unpack $ fst $ BS.breakSubstring (BSC.pack "\r\n") request
    putStrLn ("Request line: " ++ reqLine)

    -- Extract Method, URI, and HTTP Version
    let (method, rawUri, httpVer) = unpackReqLine reqLine
            
    putStrLn ("method: " ++ method ++ "\nrawUri: " ++ rawUri ++ "\nhttpVer: " ++ httpVer)

    if not (checkHeadSlash rawUri) then do
        -- No leading slash, malformed
        send400 sock
        return ("", "")
    else if (httpVer /= "HTTP/1.1" && httpVer /= "HTTP/1.0") then do
        -- Unsupported HTTP version
        send505 sock
        return ("", "")
    else if not (method `elem` supportedMethods) then do
        -- Unsupported method
        send501 sock
        return ("", "")
    else do
        -- So far so good, condense slashes and hand it off
        return (method, condenseSlashes rawUri)

        
-- if target is a directory you return index.html of that directory, if no exist then 404
--if no index.html, generate one using 'ls' and make an html page that lets you navigate the files and folders

-- 403 forbidden if the pat escapes the root directory (maybe handle it in another function?)

