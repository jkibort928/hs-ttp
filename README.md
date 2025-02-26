# hs-ttp
Making a basic web server in Haskell because of an ignorant tiktok I saw that said you couldn't

# INSTALLATION
- Install ghc using ghcup if you haven't already
- Run 'make' in the repo directory
- You now have a binary for spoober located at src/bin/Main
- Move and rename src/bin/Main as you like

# USAGE
	server [OPTIONS] <DIRECTORY>

	OPTIONS: 
		-h:
		--help: 	Display this help message

		-p:
		--port:		Specify a port

	
	DIRECTORY:
		The directory to be used as the root of the HTTP server.
		All subfolders within this directory will be accessible to the server's clients.


This program will ...
