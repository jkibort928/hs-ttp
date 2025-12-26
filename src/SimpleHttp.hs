module SimpleHttp ( doHttp ) where

-- Library Imports
import Data.Int
import Data.Char ( isControl )
import Data.List ( sort, intercalate )
import Data.List.Split ( splitOn )
import Data.Time
import Control.Monad ( unless )
import System.Directory ( doesFileExist, doesDirectoryExist, getFileSize, makeAbsolute, canonicalizePath, listDirectory )
import System.Posix.Files ( fileAccess )
import System.Timeout ( timeout )
import Network.Socket ( Socket, SockAddr )
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

-- Header recv timeout (in microseconds)
headerTimeout :: Int
headerTimeout = 10000000 -- 10 seconds to send entire header

-- Chunk send timeout (in microseconds)
sendTimeout :: Int
sendTimeout = 30000000 -- 30 seconds to receive a chunk

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

-- Format as "YYYY-MM-DD HH:MM:SS"
getTimeStamp :: IO String
getTimeStamp = formatTime defaultTimeLocale "%F %T" <$> getZonedTime
-- %F = %Y-%m-%d, %T = %H:%M:%S

-- Stops reading once it sees \r\n\r\n, or EOF
readRequest :: Socket -> IO BS.ByteString
readRequest sock = do
    result <- timeout headerTimeout (getHeaders BS.empty 0)
    case result of
        Nothing -> return BS.empty -- Timeout occured (slowloris protection)
        Just bs -> return bs
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
    --putStrLn ("FULL REQUEST:\n" ++ show request)
    --putStrLn "----------------------------------"
    
    -- Decode the request line
    let reqLine = BSC.unpack $ fst $ BS.breakSubstring (BSC.pack "\r\n") request
    --putStrLn ("Request line: " ++ reqLine)

    -- Extract Method, URI, and HTTP Version
    let (method, rawUri, httpVer) = unpackReqLine reqLine
            
    --putStrLn ("method: " ++ method ++ "\nrawUri: " ++ rawUri ++ "\nhttpVer: " ++ httpVer)

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
        -- So far so good, disregard any queries because they are not utilized
        -- (drop all text after a question mark)
        let unqueried = takeWhile (\c -> c /= '?') rawUri

        -- Decode percent encoding.
        let unescaped = unEscapeString unqueried

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

        -- Resolve symlinks for the true size of the file
        canonPath <- canonicalizePath filePath
        fileSize <- getFileSize (canonPath)

        -- TODO: Let the response be interchangeable so this function can be used to send 404.html?
        let header = BSC.pack ("HTTP/1.1 200 OK\r\nConnection: close\r\nContent-Length: " ++ (show fileSize) ++ "\r\n\r\n")
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
                -- Wrap the send in a timeout ("slow read" attack mitigation)
                result <- timeout sendTimeout $ sendAll sock' (BSLC.toStrict chunk) -- Convert the chunk to strict and send it
                case result of
                    Nothing -> return () -- Client stopped reading (read too slowly), exit loop
                    Just () -> sendChunks sock' rest -- No timeout, continue chunks

-- Sends a generated HTML file representing the list of files
-- Path is relative to the server root
sendHtmlIndex :: String -> [String] -> Socket -> IO ()
sendHtmlIndex path contents sock = do

    let generatedPage = BSC.pack $ htmlBegin ++ htmlList ++ htmlEnd
    let htmlSize = BS.length generatedPage
    let header = BSC.pack ("HTTP/1.1 200 OK\r\nConnection: close\r\nContent-Length: " ++ (show htmlSize) ++ "\r\n\r\n")
    sendAll sock header
    sendAll sock generatedPage

    where
        htmlBegin = "<!DOCTYPE html><html lang=\"en\"><head><meta http-equiv=\"content-type\" content=\"text/html; charset=UTF-8\"><meta charset=\"UTF-8\"><meta name=\"viewport\" content=\"width=device-width,initial-scale=1\"><meta http-equiv=\"X-UA-Compatible\" content=\"ie=edge\"><title>Index</title></head><body><h1>Index</h1>"
        htmlList = concatMap (collapseSlashes . (\str -> "<a href=\"" ++ path ++ "/" ++ str ++ "\">" ++ str ++ "</a><br>")) newContents
        htmlEnd = "</body></html>\n" 
        -- If not root we want to prepend a "../" entry
        newContents = if path == "/" then contents else ("../":contents)
        -- Html is sensitive to double slashes for some reason, so we collapse all consecutive slashes into one
        collapseSlashes :: String -> String
        collapseSlashes str = reverse (helper str [])
            where
                helper []           res = res
                helper ('/':'/':cs) res = helper ('/':cs)   res
                helper (c:cs)       res = helper cs         (c:res)
        
-- Perform proper checking before calling sendFile to send the file to the client over http
respond :: (String, String) -> String -> Socket -> [String] -> IO ()
respond (method, filePath) root sock flags = do

    if method == "" then do
        return ()
    else do

        let isHead = method == "HEAD"

        case collapsePath filePath of
            Nothing -> do
                -- Send 403 forbidden if the path is not good
                send403 sock
                return ()

            Just collapsedPath -> do

                -- Attach absolute root to the collapsed path
                absRoot <- makeAbsolute root
                let absFilePath = absRoot ++ ('/':collapsedPath)

                --putStrLn ("collapsedPath: " ++ collapsedPath)
                --putStrLn ("absFilePath: " ++ absFilePath)
                
                theFileExists <- doesFileExist absFilePath
                if (theFileExists) then do
                    -- File exists, send it
                    --putStrLn "Exists, sending..."
                    sendFile isHead absFilePath sock
                    return ()
                else do
                    -- File doesn't exist, check if directory
                    --putStrLn "Doesn't exist, checking if dir..."
                    theDirExists <- doesDirectoryExist absFilePath
                    if (theDirExists) then do
                        -- Directory exists, check for index.html
                        --putStrLn "Dir exists, checking for index..."
                        let indexFilePath = absFilePath ++ "/index.html"
                        indexExists <- doesFileExist indexFilePath
                        if (indexExists) then do
                            -- Index exists, send it
                            --putStrLn "Index exists"
                            sendFile isHead indexFilePath sock
                            return ()
                        else do
                            -- Directory exists, but has no index
                            --putStrLn "Dir exists, index does not, sending generated page"
                            dirList <- listDirectory absFilePath
                            dirList' <- mapM (dirSlash absFilePath) dirList
                            
                            sendHtmlIndex filePath (sort dirList') sock -- NOT absolute path as first arg. We want relative to the server root.
                            return ()
                    else do
                        -- Neither directory nor file exist
                        --putStrLn "Neither dir nor file exists"
                        send404 sock
                        return ()
    where
        -- Collapses traversals ("..")
        -- A path is invalid if it traverses past the server root at any point
        -- A path is also invalid if it contains hidden files when not allowed
        -- Returns "" if path invalid, else returns the path with collapsed traversal.
        collapsePath :: String -> Maybe String
        collapsePath path = helper (splitOn "/" path) []
            where
                helper :: [String] -> [String] -> Maybe String
                helper [] stack             = Just (intercalate "/" (reverse stack)) -- Return final result path
                helper (x:xs) stack
                    | x == "." || x == ""   = helper xs stack -- nop
                    | x == ".."             = case stack of
                        []      -> Nothing -- Terminate and return invalid if we backwards traverse when stack empty
                        (_:s)   -> helper xs s -- pop off the stack when we backwards traverse
                    | (head x) == '.' && not ("serve-dotfiles" `elem` flags) = Nothing -- Prevent serving of dotfiles unless allowed by flags (terminate and return null)
                    | otherwise             = helper xs (x:stack) -- Push to stack

        -- Appends a / to the end of an item if it is a directory.
        -- The prePath should be the absolutepath to the directory that the item is in.
        dirSlash :: String -> String -> IO String
        dirSlash prePath item = do
            isDir <- doesDirectoryExist (prePath ++ "/" ++ item)
            return (if isDir then item ++ "/" else item)
            

---------- Exported -----------

doHttp :: String -> Socket -> SockAddr -> [String] -> IO ()
doHttp root sock cliAddr flags = do
    decoded <- httpDecode sock
    
    -- Concise log
    timestamp <- getTimeStamp
    putStrLn (timestamp ++ " " ++ show cliAddr ++ ": " ++ fst decoded ++ " " ++ snd decoded)
    
    respond decoded root sock flags
    
-- TODO: Add functionality for a commandline switch to disable generated index pages. Will 404 if you try to access a directory instead.
-- TODO: Add support for 404.html, maybe as built-in to the code and generated, or stored in root as a file.
