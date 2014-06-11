all: README.html doc

HSFILES := */*.hs */*/*.hs */*/*/*.hs

doc: $(HSFILES)
	cabal haddock --executables --internal --hyperlink-source

README.html: README.md
	pandoc -s README.md -o README.html 
