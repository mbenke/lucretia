all: README.html doc

HSFILES := */*.hs */*/*.hs */*/*/*.hs

doc: $(HSFILES)
	cabal haddock --hyperlink-source

README.html: README.md
	pandoc -s README.md -o README.html 

