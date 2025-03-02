-- Library imports
import System.IO
import System.Environment (getArgs)
import Control.Exception ( throw, Exception )
import Control.Monad ( when, unless )
import Text.Read (readMaybe)

-- Error handling
import Data.Typeable ( Typeable )
newtype Error = Error {errMsg :: String}
    deriving (Show, Typeable)
instance Exception Error

-- Custom imports
import CLIUtil (checkFlags, checkLFlags, parseArgs, isArgFlag, isArgLFlag)


-- Help message to be displayed
helpMessage :: String
helpMessage = "hi\n"

defaultPort :: Int
defaultPort = 8080


-- Reads a string into an int, and if unsuccessful, returns a fallback value
safeIntRead :: Int -> String -> Int
safeIntRead fallback str = case readMaybe str of
    Just x -> x
    Nothing -> fallback

-- Safely reads a port number from a string, returning the default port if not valid
readPort :: String -> Int
readPort = safeIntRead defaultPort

-- Get the port from the command line flags
-- Arguments are in the order: flags flagArgs longFlags longFlagArgs
-- Short flags are prioritized, but if it equals the default port, we will check longflags.
-- Invalid arguments are treated as default
parsePort :: [String] -> [String] -> [String] -> [String] -> Int
parsePort flags flagArgs lflags lflagArgs = case helper flags flagArgs of
    result  | result == defaultPort -> helper lflags lflagArgs -- Check longflags if short flags return default
            | otherwise             -> result -- Default
    where
        -- Loop through first list until you find an argFlag, then match it to the first possible flagArg
        helper [] _             = defaultPort
        helper _ []             = defaultPort
        helper (f:fs) (fa:fas) 
            | isArgFlag f || isArgLFlag f   = readPort fa
            | otherwise                     = helper fs (fa:fas)

-- Main
main :: IO ()
main = do
    args <- getArgs
    let (argv, flags, longFlags, flagArgs, longFlagArgs) = parseArgs args

    if ("h" `elem` flags) || ("help" `elem` longFlags) then do
            putStrLn helpMessage
    else do
        when (null argv)                $ throw (Error "Error: No arguments specified")
        unless (checkFlags flags)       $ throw (Error "Error: Invalid flag")
        unless (checkLFlags longFlags)  $ throw (Error "Error: Invalid long flag")

        -- Extract the first argument of argv as the root directory path. Ignore other arguments.
        let (rootDir:arguments) = argv

        putStrLn ("rootDir: " ++ rootDir);
        putStrLn ("arguments: " ++ concat arguments);
        putStrLn ("flags: " ++ concat flags);
        putStrLn ("flagArgs: " ++ concat flagArgs);
        putStrLn ("longFlags: " ++ concat longFlags);
        putStrLn ("longFlagArgs: " ++ concat longFlagArgs);
        
        putStrLn "----------------------";

        -- Get the port
        let port = parsePort flags flagArgs longFlags longFlagArgs

        putStrLn ("port: " ++ show port);

        
