# hs-ttp

Made a basic web server in Haskell utilizing the lower-level networking socket interface.

This project was mainly created for the challenge and fun of it, as Haskell is a particularly
enjoyable and unique language to develop in.

# INSTALLATION

- Install ghc and cabal using ghcup if you haven't already
- Run 'cabal install' in the repo directory to install the binary into ~/.cabal/bin

# USAGE

    hs-ttp [OPTIONS] <DIRECTORY>
    
    OPTIONS: 
        -h:
        --help:     Display this help message
    
        -p:
        --port:        Specify a port
    
    DIRECTORY:
        The directory to be used as the root of the HTTP server.
        All subfolders within this directory will be accessible to the server's clients.
    
    This will create a basic HTTP server that has its root based in DIRECTORY.
    It can access any subfolder and file within this directory.
    It cannot access anything outside of this directory.
    
    The server binds to the wildcard address, meaning it will be accessible on any ip interface.
