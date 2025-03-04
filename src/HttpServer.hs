module HttpServer ( runServer )

import Network.Socket
import qualified Control.Exception as E
import qualified Data.List.NonEmpty as NE

-- Heavily references https://hackage.haskell.org/package/network-3.2.7.0/docs/Network-Socket.html

-- Runs the server on the given port, using server as the main function to run for each connection
runServer :: String -> (Socket -> SockAddr -> IO a) -> IO ()
runServer port server = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close loop
    where
    
        -- Resolve the hostname of the server given the port
        resolve = do
            let hints = defaultHints {
                    addrFlags = [AI_PASSIVE] -- Wildcard when no address given
                ,   addrSocketType = Stream -- TCP
            }
            addrList <- getAddrInfo (Just hints) Nothing (Just port)
            return (NE.head addrList) -- getAddrInfo never returns an empty list without an error

        -- Open the socket on address addr
        -- If openSocket errors, we close it, if not, we call setupSock on it
        open addr = E.bracketOnError (openSocket addr) close setupSock

        -- Setup the socket after it is opened and set it to listen
        setupSock sock = do
        
            -- Basic socket setup options
            setSocketOption sock ReuseAddr 1
            withFdSocket sock setCloseOnExecIfNeeded

            -- Bind?? I thought this was already done when we used (openSocket addr) above??
            --bind sock $ addrAddress addr

            -- Put the socket in listening mode
            -- Set a high listener queue size (larger than most systems' max)
            -- to use the largest queue that the system allows
            listen sock 1024

            return sock

        -- Listener accept loop
        -- Forever calls "accept sock"
        -- On fail it will close the connection (the connection is first value of the tuple returned by accept)
        -- On success, it will call handleConn on it
        loop sock = forever $ E.bracketOnError (accept sock) (close . fst) handleConn

        -- Handle each connection, spawning a thread
        -- We want to return nothing so we use void to discard the threadId from forkFinally
        -- We use forkFinally to start the server, and give it a cleanup function to run when the thread ends
        -- We use const because forkFinally expects a function,
        --   but we don't want to take an argument when we gracefully close.
        -- Const just "eats" an argument essentially
        handleConn (conn, peer) = void $ forkFinally (server conn peer) (const $ gracefulClose conn 5000)
