all:
	ghc -dynamic Main

FORCE:

AutoMake-rules.md: FORCE
	pandoc --number-sections --listings rules.md -o rules.pdf
