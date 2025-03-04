module CLIUtil (checkFlags, checkLFlags, parseArgs, isArgFlag, isArgLFlag) where

import Control.Exception ( throw, Exception )

-- Error handling
import Data.Typeable ( Typeable )
newtype Error = Error {errMsg :: String}
    deriving (Show, Typeable)
instance Exception Error

-- Configuration

-- Define valid flags
possibleFlags :: [Char]
possibleFlags = ['h','p']
possibleLFlags :: [String]
possibleLFlags = ["help", "port"]

-- Specify which flags take an argument
argFlags :: [Char]
argFlags = ['p']
argLFlags :: [String]
argLFlags = ["port"]


-- CLI Boilerplate code

-- Returns false if there is a flag that is not valid
checkFlags :: [String] -> Bool
checkFlags [] = True
checkFlags (s:ss) = case s of
    (f:[])      -> f `elem` possibleFlags && checkFlags ss
    otherwise   -> False

-- Returns false if there is a longflag that is not valid
checkLFlags :: [String] -> Bool
checkLFlags []          = True
checkLFlags (lf:lfs)    = lf `elem` possibleLFlags && checkLFlags lfs

-- Returns true if the string is a flag
--  (starts with a dash, followed by any character that is not a dash)
isFlag :: String -> Bool
isFlag "--"     = False
isFlag ('-':_)  = True
isFlag _        = False

-- Returns true if and only if the string is a longflag (starts with two dashes)
isLFlag :: String -> Bool
isLFlag "--"            = False
isLFlag ('-':'-':_) = True
isLFlag _           = False

-- Returns true if the string is a flag that takes an argument
isArgFlag :: String -> Bool
isArgFlag "-"           = False
isArgFlag ('-':c:[])    = c `elem` argFlags -- with dash
isArgFlag (c:[])        = c `elem` argFlags -- without dash
isArgFlag _             = False

-- Returns true if the string is a longflag that takes an argument
isArgLFlag :: String -> Bool
isArgLFlag []   = False
isArgLFlag str  = (removeDashes str) `elem` argLFlags

-- Removes all dashes from the beginning of a string (strips a flag or longflag)
removeDashes :: String -> String
removeDashes ('-':cs)   = removeDashes cs
removeDashes str    = str

-- Reverses each list within a 5tuple
reverse5 :: ([a], [b], [c], [d], [e]) -> ([a], [b], [c], [d], [e])
reverse5 (l1, l2, l3, l4, l5) = (reverse l1, reverse l2, reverse l3, reverse l4, reverse l5)

-- Parse a list of strings into a 5tuple of strings containing:
--  List of arguments
--  List of flags
--  List of longflags
--  List of flagArgs in order
--  List of lflagArgs in order
parseArgs :: [String] -> ([String], [String], [String], [String], [String])
parseArgs [] = throw (Error "Error: No arguments specified")
parseArgs x = reverse5 (parseRawArgs x) where
    -- Lists will be reversed!
    parseRawArgs strs = helper strs [] [] [] [] [] True
        where
            helper :: [String] -> [String] -> [String] -> [String] -> [String] -> [String] -> Bool -> ([String], [String], [String], [String], [String])
            helper args argv fs lfs fargs lfargs processFlags = case args of
                (s1:s2:ss)
                    -- double dash signifies we should stop processing subsequent flags and treat them as normal strings
                    | s1 == "--"                                    -> helper (s2:ss)   argv        fs                      lfs                         fargs       lfargs      False
                    -- Process long argflag s1 with argument s2
                    | processFlags && isLFlag s1 && isArgLFlag s1   -> helper ss        argv        fs                      ((removeDashes s1):lfs)     fargs       (s2:lfargs) processFlags
                    -- Process long non-arg flag s1
                    | processFlags && isLFlag s1                    -> helper (s2:ss)   argv        fs                      ((removeDashes s1):lfs)     fargs       lfargs      processFlags
                    -- Process short argflag s1 with argument s2
                    | processFlags && isFlag s1 && isArgFlag s1     -> helper ss        argv        ((removeDashes s1):fs)  lfs                         (s2:fargs)  lfargs      processFlags
                    -- Process short non-arg flag s1
                    | processFlags && isFlag s1                     -> helper (s2:ss)   argv        ((removeDashes s1):fs)  lfs                         fargs       lfargs      processFlags
                    -- Process normal argument
                    | otherwise                                     -> helper (s2:ss)   (s1:argv)   fs                      lfs                         fargs       lfargs      processFlags
                (s:[])
                    -- Note that arg flags in the last position will cause there to be an insufficient number of args in the arg list
                    -- Process longflag
                    | processFlags && isLFlag s                     ->                  (argv,      fs,                     ((removeDashes s):lfs),     fargs,      lfargs)
                    -- Process shortflag
                    | processFlags && isFlag s                      ->                  (argv,      ((removeDashes s):fs),  lfs,                        fargs,      lfargs)
                    -- Process arg
                    | otherwise                                     ->                  ((s:argv),  fs,                     lfs,                        fargs,      lfargs)
                [] -> (argv, fs, lfs, fargs, lfargs)
