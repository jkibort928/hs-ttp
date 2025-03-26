module SimpleHttp ( doHttp ) where

-- Library Imports
import Data.Int
import Data.Char ( isControl )
import Data.List.Split ( splitOn )
import Control.Monad ( unless )
import System.Directory ( doesFileExist, doesDirectoryExist, getFileSize, makeAbsolute, canonicalizePath, listDirectory )
import System.Posix.Files ( fileAccess )
import Network.Socket ( Socket )
import Network.Socket.ByteString ( recv, sendAll )
import Network.URI ( unEscapeString )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC ( pack, unpack )
import qualified Data.ByteString.Char8 as BSLC ( toStrict )

bufferSize :: Int
bufferSize = 1024

chunkSize :: Int64 -- ByteString.Lazy ( splitAt ) needs it to be this way 
chunkSize = 4096

maxHeaderLength :: Int
maxHeaderLength = 16384

------- Configuration ---------
supportedMethods :: [String]
supportedMethods = ["GET", "HEAD"]

--- HTTP Error Status Codes ---

-- Malformed request
send400 :: Socket -> IO ()
send400 sock = sendAll sock $ BSC.pack "HTTP/1.1 400 Bad Request\r\n\r\n"

-- File is not readable or is outside the server root
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


    
-- Returns (method, filepath)
-- Empty method string signifies an error has already been sent to the client
httpDecode :: Socket -> IO (String, String)
httpDecode sock = do

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
        -- So far so good, decode percent encoding.
        let unescaped = unEscapeString rawUri

        -- Check for control characters
        if ( any isControl unescaped ) then do
            send400 sock
            return ("", "")
        else
            return (method, unescaped)

    where
        -- Breaks up the request line by spaces into a triple
        unpackReqLine :: String -> (String, String, String)
        unpackReqLine str = (fst split1, fst split2, (drop 1) $ snd split2)
            where 
                split1 = break (' '==) str
                split2 = break (' '==) ((drop 1) $ snd split1)
    
        -- Checks if the string begins with a /
        checkHeadSlash :: String -> Bool
        checkHeadSlash str = case str of
            (c:_)  -> c == '/'
            []      -> False
        

-- Sends the requested file, crafting the HTTP request
sendFile :: Bool -> String -> Socket -> IO ()
sendFile isHead filePath sock = do

    hasAccess <- fileAccess filePath True False False

    -- Check file access 
    if not hasAccess then do
        -- Send 403 forbidden, cannot read file
        send403 sock
        return ()
    else do

        canonPath <- canonicalizePath filePath
    
        -- Resolve symlinks for the true size of the file
        fileSize <- getFileSize (canonPath)

        -- TODO: Let the response be interchangeable so this function can be used to send 404.html?
        let header = BSC.pack ("HTTP/1.1 200 OK\nContent-Length: " ++ (show fileSize) ++ "\r\n\r\n")
        sendAll sock header

        if isHead then do
            return ()
        else do
            -- Read the file lazily
            fileContents <- BSL.readFile filePath
            -- Send in chunks over the socket
            sendChunks sock fileContents
    where
        sendChunks :: Socket -> BSL.ByteString -> IO ()
        sendChunks sock' content = do
            let (chunk, rest) = BSL.splitAt chunkSize content -- Split the content into chunkSize sized chunks
            unless (BSL.null chunk) $ do -- Stop if we ran out
                sendAll sock' (BSLC.toStrict chunk) -- Convert the chunk to strict and send it
                sendChunks sock' rest -- Send the rest
        
-- Perform proper checking before calling sendFile to send the file to the client over http
respond :: (String, String) -> String -> Socket -> IO ()
respond (method, filePath) root sock = do

    if method == "" then do
        return ()
    else do

        let isHead = method == "HEAD"

        if (willEscapeRoot filePath) then do
            -- Send 403 forbidden if the path escapes the server root
            send403 sock
            return ()
        else do
            -- File is within server root dir
            
            absRoot <- makeAbsolute root
            let absFilePath = absRoot ++ ('/':filePath)

            putStrLn absFilePath
            
            theFileExists <- doesFileExist absFilePath
            if (theFileExists) then do
                -- File exists, send it
                putStrLn "Exists, sending..."
                sendFile isHead absFilePath sock
                return ()
            else do
                -- File doesn't exist, check if directory
                putStrLn "Doesn't exist, checking if dir..."
                theDirExists <- doesDirectoryExist absFilePath
                if (theDirExists) then do
                    -- Directory exists, check for index.html
                    putStrLn "Dir exists, checking for index..."
                    let indexFilePath = absFilePath ++ "/index.html"
                    indexExists <- doesFileExist indexFilePath
                    if (indexExists) then do
                        -- Index exists, send it
                        putStrLn "Index exists"
                        sendFile isHead indexFilePath sock
                        return ()
                    else do
                        -- Directory exists, but has no index
                        putStrLn "Dir exists, index does not, sending generated page"
                        -- TODO: Implement a generated page based off getDirectoryContents (use Data.List sort on it)
                        lsList <- listDirectory absFilePath
                        --sendAll sock $ BSC.pack $ (concat lsList)
                        putStrLn $ show (concat lsList)
                        send404 sock
                        return ()
                else do
                    -- Neither directory nor file exist
                    putStrLn "Neither dir nor file exists"
                    send404 sock
                    return ()
    where
        -- Checks if the path will escape the root directory
        willEscapeRoot :: String -> Bool
        willEscapeRoot path = helper (splitOn "/" path) 0
            where
                helper :: [String] -> Integer -> Bool
                helper (x:xs) depth
                    | depth < 0             = True -- If it escapes at any time, return true
                    | x == ".."             = helper xs (depth - 1)
                    | x == "." || x == ""   = helper xs depth
                    | otherwise             = helper xs (depth + 1)
                helper [] depth = depth < 0
            

---------- Exported -----------

doHttp :: String -> Socket -> IO ()
doHttp root sock = do
    decoded <- httpDecode sock
    respond decoded root sock
    
