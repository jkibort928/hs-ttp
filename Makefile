all: Main

Main: src/Main.hs src/bin src/build
	ghc --make src/Main.hs -o src/bin/Main -odir src/build -hidir src/build -isrc

src/bin:
	mkdir -p src/bin

src/build:
	mkdir -p src/build

clean:
	rm src/bin/* src/build/*
