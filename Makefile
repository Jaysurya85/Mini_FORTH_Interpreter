EXEC = FORTH
CABAL = cabal
	
build:
	$(CABAL) build

# Usage: make run FILE=tests/t1.4TH
run:
	$(CABAL) run $(EXEC) -- tests/$(FILE).4TH

run_all:
	$(CABAL) run $(EXEC) -- tests/t1.4TH
	$(CABAL) run $(EXEC) -- tests/t2.4TH
	$(CABAL) run $(EXEC) -- tests/t3.4TH
	$(CABAL) run $(EXEC) -- tests/t4.4TH
	$(CABAL) run $(EXEC) -- tests/t5.4TH
	$(CABAL) run $(EXEC) -- tests/t6.4TH
	$(CABAL) run $(EXEC) -- tests/t7.4TH
	$(CABAL) run $(EXEC) -- tests/t8.4TH
	$(CABAL) run $(EXEC) -- tests/t9.4TH
	$(CABAL) run $(EXEC) -- tests/t10.4TH

unit:
	runhaskell ValSpec.hs
	runhaskell EvalSpec.hs
	runhaskell InterpretSpec.hs

clean:
	$(CABAL) clean
	rm -rf dist-newstyle
