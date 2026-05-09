module Main (main) where

-- Library imports
import System.Environment ( getArgs )
import System.Exit ( exitSuccess )
import Control.Exception ( throw, Exception )
import Control.Monad ( unless, when )

-- Custom imports
import Version ( serverVersion )
import CLIUtil ( checkFlags, checkOpts, parseArgs, getOpt )
import TCPServer ( runServer )
import SimpleHttp ( doHttp )

-- Error handling
import Data.Typeable ( Typeable )
newtype Error = Error {errMsg :: String}
    deriving (Show, Typeable)
instance Exception Error

-- Help message to be displayed
helpMessage :: String
helpMessage = "hs-ttp [OPTIONS] <DIRECTORY>\n\n[OPTIONS]: \n    -h:\n    --help:             Display this help message\n\n    --version:          Display the server version\n\n    -p:\n    --port:             Specify a port\n\n    --serve-dotfiles:   Allow the server to serve hidden files (files that begin with a period)\n\n    --no-index:         Disable auto-generated index pages for directories lacking index.html files\n\n<DIRECTORY>:\n    The directory to be used as the root of the HTTP server.\n    All subfolders within this directory will be accessible to the server's clients.\n\nThis will create a basic HTTP server that has its root based in DIRECTORY.\nIt can access any subfolder and file within this directory.\nIt cannot access anything outside of this directory.\nBy default, the server will provide an auto-generated HTML index page for all directories lacking an index.html file.\n\nThe server binds to the wildcard address, meaning it will be accessible on any ip interface.\n"
defaultPort :: String
defaultPort = "8080"

-- Helpers
headSafe :: [String] -> String
headSafe [] = ""
headSafe (str:_) = str

-- Main
main :: IO ()
main = do
    args <- getArgs
    let (argv, flags, opts, optArgs) = parseArgs args

    when (("h" `elem` flags) || ("help" `elem` flags)) $ do
        putStrLn helpMessage
        exitSuccess
    when ("version" `elem` flags) $ do
        putStrLn $ "hs-ttp v" ++ serverVersion
        exitSuccess
        
    when (null argv) $ 
        throw (Error "Error: No arguments specified")
    unless (checkFlags flags) $ 
        throw (Error "Error: Invalid flag")
    unless (checkOpts opts optArgs) $ 
        throw (Error "Error: Invalid options")    

    let rootDir = head argv
        port    = getOpt ["p", "port"] defaultPort opts optArgs
        
    runServer port (serverFunc flags) [rootDir]
        where
            serverFunc flags servArgs sock cliAddr = do
                let root = headSafe servArgs
                doHttp root sock cliAddr flags
