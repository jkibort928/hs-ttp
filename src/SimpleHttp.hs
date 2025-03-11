module SimpleHttp ( simpleHttpDecode ) where

-- Library Imports
import Network.Socket ( Socket )
import Network.Socket.ByteString ( recv, sendAll )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC ( pack, unpack )

bufferSize :: Int
bufferSize = 1024

maxHeaderLength :: Int
maxHeaderLength = 16384

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

---------- Helpers ------------

-- Stops reading once it sees \r\n\r\n, or EOF
-- I have no idea how to avoid this nesting
readRequest :: Socket -> IO BS.ByteString
readRequest sock = getHeaders BS.empty 0
    where
        getHeaders :: BS.ByteString -> Int -> IO BS.ByteString
        getHeaders buff bytesRead = do
            chunk <- recv sock bufferSize

            -- Force a 400 bad request if length exceeded
            let newBytesRead = bytesRead + BS.length chunk
            if newBytesRead > maxHeaderLength
                then return BS.empty

                -- EOF, return what we have
                else if BS.null chunk
                    then return buff
                    else do
                        -- Append new chunk to buffer
                        let newBuffer = buff `BS.append` chunk
                        -- Check for \r\n\r\n
                        if hasHeaderEnd newBuffer
                            -- Return the final buffer, not caring if more data is after header
                            then return newBuffer
                            -- Continue reading the header
                            else getHeaders newBuffer newBytesRead

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

-- Ensures no "naughtiness" in the unix filepath 
-- Prevents backtracking, and other inappropriate actions that might compromise security
-- Placeholder implementation
sanitizePath :: String -> String
sanitizePath path = path

---------- Exported -----------

-- Returns (method, filepath)
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

    -- method only GET or HEAD

    -- URI requires leading slash, ignores trailing slash,
    --and if target is a directory you return index.html of that directory, if no exist then 404
    --if no index.html, generate one using 'ls' and make an html page that lets you navigate the files and folders

    -- version check 1.1 or 1.0 works
    
    send501 sock

    return ("", "")
