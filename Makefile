all: install doc

HSFILES := */*.hs */*/*.hs */*/*/*.hs

install:
	cabal install

test:
	cabal test

doc: $(HSFILES) README.html
	cabal configure
	cabal haddock --hyperlink-source

README.html: README.md
	pandoc -s README.md -o README.html 

clean:
	cabal clean
	rm README.html
