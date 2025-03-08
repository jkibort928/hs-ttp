module Main (main) where

-- Library imports
import System.IO
import System.Environment (getArgs)
import Control.Exception ( throw, Exception )
import Control.Monad ( when, unless )
import Text.Read (readMaybe)

-- Custom imports
import CLIUtil ( checkFlags, checkOpts, parseArgs, getOpt )
import TCPServer ( runServer )

-- Error handling
import Data.Typeable ( Typeable )
newtype Error = Error {errMsg :: String}
    deriving (Show, Typeable)
instance Exception Error

-- Help message to be displayed
helpMessage :: String
helpMessage = "hi\n"

defaultPort :: String
defaultPort = "8080"

-- Main
main :: IO ()
main = do
    args <- getArgs
    let (argv, flags, opts, optArgs) = parseArgs args

    if ("h" `elem` flags) || ("help" `elem` flags) then do
            putStrLn helpMessage
    else do
        when (null argv)                $ throw (Error "Error: No arguments specified")
        unless (checkFlags flags)       $ throw (Error "Error: Invalid flag")

        -- Extract the first argument of argv as the root directory path. Ignore other arguments.
        let (rootDir:arguments) = argv

        -- Debug prints
        --{-
        putStrLn ("rootDir: " ++ rootDir);
        putStrLn ("arguments: " ++ concat arguments);
        putStrLn ("flags: " ++ concat flags);
        putStrLn ("opts: " ++ concat opts);
        putStrLn ("optArgs: " ++ concat optArgs);
        
        putStrLn "----------------------";
        ---}
        
        -- Get the port
        let port = getOpt ["p", "port"] "8080" opts optArgs 

        runServer port serverFunc 
            where
                serverFunc sock cliAddr = do
                    putStrLn ("Client connected from: " ++ show cliAddr)

        -- My litte to-do list:
        -- Open the listener socket DONE
        -- Recieve http request
        -- Decode http request
        -- Craft http response
            -- Read file in question
            -- Send error if not correct
            -- Maybe check if file is within the root directory
            -- Deal with ../ and such
        -- Send http response
        -- Listen again DONE
