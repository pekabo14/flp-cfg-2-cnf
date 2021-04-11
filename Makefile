
login = xkapic02
program = bkg-2-cnf
file_to_zip = src/* test/* doc/* Makefile

all:
	ghc --make -o ${program} src/Main.hs src/GrammarParser.hs src/SimpleRules.hs src/Chomsky.hs

clean:
	rm src/*.o src/*.hi ${program}

zip:
	zip flp-fun-${login}.zip ${file_to_zip}