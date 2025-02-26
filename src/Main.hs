import System.IO
import System.Environment (getArgs)
import Control.Exception ( throw, Exception )
import Control.Monad ( when, unless )

-- Custom modules
import CLIUtil (checkFlags, checkLFlags, parseArgs)

-- Error handling
import Data.Typeable ( Typeable )
newtype Error = Error {errMsg :: String}
    deriving (Show, Typeable)
instance Exception Error


-- Help message to be displayed
helpMessage :: String
helpMessage = "hi\n"

main :: IO ()
main = do
	args <- getArgs
	let (argv, flags, longFlags, flagArgs, longFlagArgs) = parseArgs args

	if ("h" `elem` flags) || ("help" `elem` longFlags) then do
	        putStrLn helpMessage
	else do
		when (null argv)					$ throw (Error "Error: No arguments specified")
		unless (checkFlags flags)		$ throw (Error "Error: Invalid flag")
		unless (checkLFlags longFlags)	$ throw (Error "Error: Invalid long flag")

		-- Extract the first argument of argv as the root directory path. Ignore other arguments.
		let (rootDir:arguments) = argv

		print ("rootDir: " ++ rootDir)
		print ("arguments: " ++ concat arguments)
        print ("flags: " ++ concat flags)
        print ("flagArgs: " ++ concat flagArgs) 
        print ("longFlags: " ++ concat longFlags)
        print ("longFlagArgs: " ++ concat longFlagArgs)
