EXEC = FORTH
CABAL = cabal
	
build:
	$(CABAL) build

# Usage: make run FILE=tests/t1.4TH
run:
	$(CABAL) run $(EXEC) -- $(FILE)

run_all:
	$(CABAL) run $(EXEC) -- tests/t1.4TH

unit:
	runhaskell ValSpec.hs
	runhaskell EvalSpec.hs
	runhaskell InterpretSpec.hs

clean:
	$(CABAL) clean
	rm -rf dist-newstyle
