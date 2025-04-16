module Main (main) where

-- Library imports
import System.Environment ( getArgs )
import Control.Exception ( throw, Exception )
import Control.Monad ( unless )

-- Custom imports
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
helpMessage = "hs-ttp [OPTIONS] <DIRECTORY>\n\nOPTIONS: \n\t-h:\n\t--help: \tDisplay this help message\n\n\t-p:\n\t--port:\t\tSpecify a port\n\nDIRECTORY:\n\tThe directory to be used as the root of the HTTP server.\n\tAll subfolders within this directory will be accessible to the server's clients.\n\nThis will create a basic HTTP server that has its root based in DIRECTORY.\nIt can access any subfolder and file within this directory.\nIt cannot access anything outside of this directory.\n\nThe server binds to the wildcard address, meaning it will be accessible on any ip interface.\n"

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

    if ("h" `elem` flags) || ("help" `elem` flags) then do
            putStrLn helpMessage
    else
    
        case argv of
            [] -> throw (Error "Error: No arguments specified")
            
            -- Extract the first argument of argv as the root directory path. Ignore other arguments.
            (rootDir:arguments) -> do

                -- Debug prints
                {-
                putStrLn ("rootDir: " ++ rootDir);
                putStrLn ("arguments: " ++ show arguments);
                putStrLn ("flags: " ++ show flags);
                putStrLn ("opts: " ++ show opts);
                putStrLn ("optArgs: " ++ show optArgs);
                
                putStrLn "----------------------";
                -}
                
                unless (checkFlags flags)       $ throw (Error "Error: Invalid flag")
                unless (checkOpts opts optArgs) $ throw (Error "Error: Invalid options")

                -- Get the port
                let port = getOpt ["p", "port"] defaultPort opts optArgs 

                runServer port serverFunc [rootDir]
                    where
                        serverFunc servArgs sock cliAddr = do
                            --putStrLn ("Server args: " ++ show servArgs)
                            --putStrLn ("Client connected from: " ++ show cliAddr)

                            let root = headSafe servArgs
                            doHttp root sock cliAddr
