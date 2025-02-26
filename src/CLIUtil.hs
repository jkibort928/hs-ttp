module CLIUtil (checkFlags, checkLFlags, parseArgs) where

import Control.Exception ( throw, Exception )

-- Error handling
import Data.Typeable ( Typeable )
newtype Error = Error {errMsg :: String}
    deriving (Show, Typeable)
instance Exception Error

-- CLI Boilerplate code

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
isArgFlag ('-':c:[])    = c `elem` argFlags
isArgFlag (c:[])        = c `elem` argFlags -- idk if needed
isArgFlag _             = False

-- Returns true if the string is a longflag that takes an argument
isArgLFlag :: String -> Bool
isArgLFlag []   = False
isArgLFlag str  = (removeDashes str) `elem` argLFlags

-- Removes all dashes from the beginning of a string (strips a flag or longflag)
removeDashes :: String -> String
removeDashes ('-':cs)   = removeDashes cs
removeDashes str    = str

-- Parse a list of strings into a 5tuple of strings containing:
--  List of arguments
--  List of flags
--  List of longflags
--  List of flagArgs
--  List of lflagArgs
-- Lists will be reversed!
parseRawArgs :: [String] -> ([String], [String], [String], [String], [String])
parseRawArgs [] = throw (Error "Error: No arguments specified")
parseRawArgs strs = helper strs [] [] [] [] [] True
    where
        helper :: [String] -> [String] -> [String] -> [String] -> [String] -> [String] -> Bool -> ([String], [String], [String], [String], [String])
        helper args argv fs lfs fargs lfargs processFlags = case args of
            (s1:s2:ss)
                | s1 == "--"                                    -> helper (s2:ss)   argv        fs                      lfs                         fargs       lfargs      False
                | processFlags && isLFlag s1 && isArgLFlag s1   -> helper ss        argv        fs                      ((removeDashes s1):lfs)     fargs       (s2:lfargs) processFlags
                | processFlags && isLFlag s1                    -> helper (s2:ss)   argv        fs                      ((removeDashes s1):lfs)     fargs       lfargs      processFlags
                | processFlags && isFlag s1 && isArgFlag s1     -> helper ss        argv        ((removeDashes s1):fs)  lfs                         (s2:fargs)  lfargs      processFlags
                | processFlags && isFlag s1                     -> helper (s2:ss)   argv        ((removeDashes s1):fs)  lfs                         fargs       lfargs      processFlags
                | otherwise                                     -> helper (s2:ss)   (s1:argv)   fs                      lfs                         fargs       lfargs      processFlags
            (s1:s2:[])
                | s1 == "--"                                    ->                  ((s2:argv), fs,                     lfs,                        fargs,      lfargs)
                | processFlags && isLFlag s1 && isArgLFlag s1   ->                  (argv,      fs,                     ((removeDashes s1):lfs),    fargs,      (s2:lfargs))
                | processFlags && isLFlag s1                    -> helper [s2]      argv        fs                      ((removeDashes s1):lfs)     fargs       lfargs      processFlags
                | processFlags && isFlag s1 && isArgFlag s1     ->                  (argv,      ((removeDashes s1):fs), lfs,                        (s2:fargs), lfargs)
                | processFlags && isFlag s1                     -> helper [s2]      argv        ((removeDashes s1):fs)  lfs                         fargs       lfargs      processFlags
                | otherwise                                     -> helper [s2]      (s1:argv)   fs                      lfs                         fargs       lfargs      processFlags
            (s:[])
                | processFlags && isLFlag s                     ->                  (argv,      fs,                     ((removeDashes s):lfs),     fargs,      lfargs)
                | processFlags && isFlag s                      ->                  (argv,      ((removeDashes s):fs),  lfs,                        fargs,      lfargs)
                | otherwise                                     ->                  ((s:argv),  fs,                     lfs,                        fargs,      lfargs)
            [] -> (argv, fs, lfs, fargs, lfargs)

reverse5 :: ([a], [b], [c], [d], [e]) -> ([a], [b], [c], [d], [e])
reverse5 (l1, l2, l3, l4, l5) = (reverse l1, reverse l2, reverse l3, reverse l4, reverse l5)

parseArgs :: [String] -> ([String], [String], [String], [String], [String])
parseArgs x = reverse5 (parseRawArgs x)

