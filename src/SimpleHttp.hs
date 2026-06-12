{-# LANGUAGE MultiWayIf #-}
module SimpleHttp ( doHttp ) where

-- Library Imports
import Data.Int
import Data.Char ( isControl )
import Data.List ( sort, intercalate )
import Data.List.Split ( splitOn )
import Data.Time
import qualified Data.Text as T
import Control.Monad ( unless )
import System.Directory ( doesFileExist, doesDirectoryExist, getFileSize, makeAbsolute, canonicalizePath, listDirectory )
import System.Posix.Files ( fileAccess )
import System.Timeout ( timeout )
import Network.Socket ( Socket, SockAddr )
import Network.Socket.ByteString ( recv, sendAll )
import Network.URI ( unEscapeString )
import Network.Mime (defaultMimeLookup)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC ( pack, unpack )
import qualified Data.ByteString.Char8 as BSLC ( toStrict )

import Version (serverHeader)

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
send400 sock = sendAll sock $ BSC.pack "HTTP/1.1 400 Bad Request\r\nContent-Length: 0\r\n\r\n"

-- File is not readable or is outside the server root
send403 :: Socket -> IO ()
send403 sock = sendAll sock $ BSC.pack "HTTP/1.1 403 Forbidden\r\nContent-Length: 0\r\n\r\n"

-- File not found (or outside the scope of the server directory)
send404 :: Socket -> IO ()
send404 sock = sendAll sock $ BSC.pack "HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\n\r\n"

-- Invalid method
send501 :: Socket -> IO ()
send501 sock = sendAll sock $ BSC.pack "HTTP/1.1 501 Not Implemented\r\nContent-Length: 0\r\n\r\n"

-- Invalid version
send505 :: Socket -> IO ()
send505 sock = sendAll sock $ BSC.pack "HTTP/1.1 505 HTTP Version Not Supported\r\nContent-Length: 0\r\n\r\n"

---------- Helpers ------------

-- File mimetypes
getMime :: String -> String
getMime path = BSC.unpack $ defaultMimeLookup (T.pack path)

-- Format as "YYYY-MM-DD HH:MM:SS"
getTimeStamp :: IO String
getTimeStamp = formatTime defaultTimeLocale "%F %T" <$> getZonedTime
-- %F = %Y-%m-%d, %T = %H:%M:%S

-- Splits a ByteString immediately after the first occurrence of a delimiter.
-- The delimiter is kept on the left side of the split.
splitAfter :: BS.ByteString -> BS.ByteString -> (BS.ByteString, BS.ByteString)
splitAfter delim buff = case BS.breakSubstring delim buff of
    (before, matchAndAfter)
        | BS.null matchAndAfter -> (buff, BS.empty)
        | otherwise             -> 
            let (match, after) = BS.splitAt (BS.length delim) matchAndAfter
            in (before `BS.append` match, after)

-- Takes socket and a starting buffer (leftover bytes after end of previous http request)
-- Returns (requestHeader, leftovers)
readRequest :: Socket -> BS.ByteString -> IO (BS.ByteString, BS.ByteString)
readRequest sock leftovers = do
    result <- timeout headerTimeout (getHeaders leftovers 0)
    case result of
        Nothing -> return (BS.empty, BS.empty) -- Timeout occured (slowloris protection)
        Just (bs, rest) -> return (bs, rest)
    where
        delim = BSC.pack "\r\n\r\n"
        getHeaders :: BS.ByteString -> Int -> IO (BS.ByteString, BS.ByteString)
        getHeaders buff bytesRead = do
            let (req, rest) = splitAfter delim buff
            if delim `BS.isSuffixOf` req then do
                return (req, rest) -- Found end of header
            else do
                -- \r\n\r\n not found yet, recev more
                chunk <- recv sock bufferSize
                let newBytesRead = bytesRead + BS.length chunk

                if newBytesRead > maxHeaderLength then return (BS.empty, BS.empty) -- Length exceeded, force a 400 error
                else if BS.null chunk then return (buff, BS.empty) -- Hit EOF, terminate recursion
                else getHeaders (buff `BS.append` chunk) newBytesRead -- All good, append new chunk to buffer and recurse

    
-- Returns (method, filepath, leftovers)
-- Empty method string signifies an error has already been sent to the client
httpDecode :: Socket -> BS.ByteString -> IO (String, String, BS.ByteString)
httpDecode sock leftovers = do
    (request, newLeftovers) <- readRequest sock leftovers
    
    let reqLine = BSC.unpack $ fst $ BS.breakSubstring (BSC.pack "\r\n") request
    let (method, rawUri, httpVer) = unpackReqLine reqLine
    let unqueried = takeWhile (\c -> c /= '?') rawUri -- (drop all text after a question mark, queries not used)
    let unescaped = unEscapeString unqueried -- Decode percent encoding.
    
    --putStrLn ("FULL REQUEST:\n" ++ show request)
    --putStrLn "----------------------------------"
    --putStrLn ("Request line: " ++ reqLine)
    --putStrLn ("method: " ++ method ++ "\nrawUri: " ++ rawUri ++ "\nhttpVer: " ++ httpVer)

    if  | BS.null request                               -> return ("", "", BS.empty)
        | not (checkHeadSlash rawUri)                   -> failWith (send400 sock)
        | httpVer `notElem` ["HTTP/1.1", "HTTP/1.0"]    -> failWith (send505 sock)
        | method `notElem` supportedMethods             -> failWith (send501 sock)
        | any isControl unescaped                       -> failWith (send400 sock)
        | otherwise                                     -> return (method, unescaped, newLeftovers)

    where
        -- Does IO action then returns empty
        failWith :: IO () -> IO (String, String, BS.ByteString)
        failWith errAction = errAction >> return ("", "", BS.empty)
    
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
    else do

        -- Resolve symlinks for the true size of the file
        canonPath <- canonicalizePath filePath
        fileSize <- getFileSize (canonPath)

        let mimeType = getMime filePath

        -- TODO: Let the response line be interchangeable so this function can be used to send 404.html?
        let header = BSC.pack $ "HTTP/1.1 200 OK\r\n" ++
                                serverHeader ++
                                "Connection: keep-alive\r\n" ++
                                "Content-Length: " ++ (show fileSize) ++ "\r\n" ++
                                "Content-Type: " ++ mimeType ++ "\r\n" ++
                                "X-Content-Type-Options: nosniff\r\n" ++
                                "\r\n"
        sendAll sock header

        unless isHead $ do
            fileContents <- BSL.readFile filePath
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
    let header = BSC.pack $ "HTTP/1.1 200 OK\r\n" ++
                            serverHeader ++
                            "Connection: keep-alive\r\n" ++
                            "Content-Length: " ++ (show htmlSize) ++ "\r\n" ++
                            "Content-Type: text/html; charset=UTF-8\r\n" ++
                            "X-Content-Type-Options: nosniff\r\n\r\n"
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
    absRoot <- makeAbsolute root

    case collapsePath filePath of
        Nothing -> send403 sock -- Forbidden; Invalid path or hidden file

        Just collapsedPath -> do
            let absFilePath = absRoot ++ ('/':collapsedPath)
            let isHead = method == "HEAD"
            
            isFile      <- doesFileExist absFilePath
            isDir       <- doesDirectoryExist absFilePath

            --putStrLn ("collapsedPath: " ++ collapsedPath)
            --putStrLn ("absFilePath: " ++ absFilePath)

            if  | isFile    -> sendFile isHead absFilePath sock
                | isDir     -> serveDirectory absFilePath isHead
                | otherwise -> send404 sock

    where

        isRestricted :: String -> Bool
        isRestricted ('.':_) = "serve-dotfiles" `notElem` flags
        isRestricted _       = False
                
        serveDirectory :: String -> Bool -> IO ()
        serveDirectory absPath isHead = do
            let indexPath = absPath ++ "/index.html"
            hasIndex <- doesFileExist indexPath
            if  | hasIndex  -> sendFile isHead indexPath sock
                | "no-index" `elem` flags -> send404 sock
                | otherwise -> sendGeneratedIndex absPath

        -- Generates and sends a file-browser style index.html
        sendGeneratedIndex :: String -> IO ()
        sendGeneratedIndex absPath = do
            dirListRaw  <- listDirectory absPath
            let dirList = filter (not . isRestricted) dirListRaw
            
            dirList'    <- mapM (dirSlash absPath) dirList
            sendHtmlIndex filePath (sort dirList') sock -- Relative to server root, not absolute paths

    
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
                    | isRestricted x        = Nothing -- Prevent serving of dotfiles (terminate and return null)
                    | otherwise             = helper xs (x:stack) -- Push to stack

        -- Appends a / to the end of an item if it is a directory.
        -- The prePath should be the absolutepath to the directory that the item is in.
        dirSlash :: String -> String -> IO String
        dirSlash prePath item = do
            isDir <- doesDirectoryExist (prePath ++ "/" ++ item)
            return (if isDir then item ++ "/" else item)
            

---------- Exported -----------

doHttp :: String -> Socket -> SockAddr -> [String] -> IO ()
doHttp root sock cliAddr flags = loop BS.empty
    where
        loop leftovers = do
            (method, path, newLeftovers) <- httpDecode sock leftovers
            unless (null method) $ do
                timestamp <- getTimeStamp
                putStrLn (timestamp ++ " " ++ show cliAddr ++ ": " ++ method ++ " " ++ path)

                respond (method, path) root sock flags

                -- Recursively wait for the next request on the same socket
                loop newLeftovers 
    
-- TODO: Add functionality for a commandline switch to disable generated index pages. Will 404 if you try to access a directory instead.
-- TODO: Add support for 404.html, maybe as built-in to the code and generated, or stored in root as a file.
