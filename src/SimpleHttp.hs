module SimpleHttp ( simpleHttpDecode ) where

import Network.Socket ( Socket )
import Network.Socket.ByteString ( recv, sendAll )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC ( pack )
import Control.Monad ( when )

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
                
            

---------- Exported -----------

-- Returns (method, filepath)
simpleHttpDecode :: Socket -> IO (String, String)
simpleHttpDecode sock = do
    -- Recieve the request
    request <- readRequest sock
    putStrLn ( "REQUEST:\n" ++ show request)
    
    -- Decode the header
    
    send501 sock

    return ("", "")
