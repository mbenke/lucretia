all: lucre README.html doc

HSFILES := */*.hs */*/*.hs

lucre: lucre.hs $(HSFILES)
	ghc --make lucre.hs -XTypeSynonymInstances

doc: $(HSFILES)
	cabal haddock --executables --internal --hyperlink-source

README.html: README.md
	pandoc -s README.md -o README.html 
