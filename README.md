# hs-ttp

A basic web server in Haskell utilizing the lower-level networking socket interface.

This project was mainly created for the challenge and fun of it, as Haskell is a particularly
enjoyable and unique language to develop in.

# INSTALLATION

- [Install ghc and cabal using ghcup](https://www.haskell.org/ghcup/) if you haven't already
- Run 'cabal install' in the repo directory to install the binary into ~/.cabal/bin

# USAGE

    hs-ttp [OPTIONS] <DIRECTORY>

    [OPTIONS]: 
        -h:
        --help:             Display this help message

        -p:
        --port:             Specify a port

        --serve-dotfiles:   Allow the server to serve hidden files (files that begin with a period)

        -<wip>:
        --<wip>:            Disable auto-generated index pages for directories lacking index.html files

    <DIRECTORY>:
        The directory to be used as the root of the HTTP server.
        All subfolders within this directory will be accessible to the server's clients.

    This will create a basic HTTP server that has its root based in DIRECTORY.
    It can access any subfolder and file within this directory.
    It cannot access anything outside of this directory.
    By default, the server will provide an auto-generated HTML index page for all directories lacking an index.html file.

    The server binds to the wildcard address, meaning it will be accessible on any ip interface.
