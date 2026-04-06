# Makefile for PLT lab 2 in Haskell

## Variables
###########################################################################

# Files of solution.  Add more files as needed.
files=src/kx.cf line.cabal src/Types.hs src/Main.hs

## Building
###########################################################################

.PHONY: line
line: $(files)
	stack install --local-bin-path=.

# Running a test
###########################################################################

# Run a single test, e.g. good/large_const.cc
# .PHONY: test
# test:
# 	stack run --stack-yaml=testsuite/stack.yaml -- -g testsuite/good/large_const.cc .

# Rules for cleaning generated files
###########################################################################

.PHONY: clean
	stack clean

# EOF
