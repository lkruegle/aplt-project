# Makefile for PLT lab 2 in Haskell

## Variables
###########################################################################

# Files of solution.  Add more files as needed.
files=src/Grammar.cf aplt.cabal

## Building
###########################################################################

.PHONY: lab2
lab2: $(files)
	stack install --local-bin-path=.
#	cabal install --installdir=. --install-method=copy --overwrite-policy=always

# Running a test
###########################################################################

# Run a single test, e.g. good/large_const.cc
.PHONY: test
test:
	stack run --stack-yaml=testsuite/stack.yaml -- -g testsuite/good/large_const.cc .

# Rules for cleaning generated files
###########################################################################

.PHONY: clean
	stack clean
#	cabal clean

# EOF
