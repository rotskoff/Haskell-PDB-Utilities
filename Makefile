GHCMAKE    = ghc --make -O
RUNHASKELL = runhaskell

default: 
	@echo "This Makefile remains for historical reasons, and"
	@echo "For most purposes, you should use 'cabal-install' instead."
	@echo "But if you insist, please provide a make target"

clean:
	find . -name *.o -o -name *.hi -o -name *.p_o -o -name *.p_hi | xargs rm

user_install: user_conf build haddock user_inst

install: conf build haddock inst

user_conf:
	$(RUNHASKELL) Setup.hs configure -p --user --prefix=${HOME}

build:
	$(RUNHASKELL) Setup.hs build 

haddock: 
	$(RUNHASKELL) Setup.hs haddock

user_inst:
	$(RUNHASKELL) Setup.hs install --user

conf:
	$(RUNHASKELL) Setup.hs configure -p 

inst:
	sudo $(RUNHASKELL) ./Setup.hs install

test:
	$(GHCMAKE) Test.hs -o qc
	./qc

test_hpc:
	$(GHCMAKE) -fhpc Test.hs -o qc.hpc
	./qc.hpc
	hpc report qc.hpc

# bench:
# 	$(GHCMAKE) Bench.hs -o qb
# 	@echo ==================== >> benchmark.log
# 	@echo -n Start: >> benchmark.log
# 	@date >> benchmark.log
# 	@uname -a >> benchmark.log
# 	@echo ghc version: `strings qb | grep '^[6-9]\.[0-9]'` >> benchmark.log
# 	@echo $(GHCMAKE) >> benchmark.log
# 	./qb +RTS -sbenchmark.gc | tee -a benchmark.log
# 	@echo -n End: >> benchmark.log
# 	@date >> benchmark.log

# bench_hpc:
# 	$(GHCMAKE) Bench.hs -o qb.hpc
# 	./qb.hpc
# 	hpc report qb.hpc

update:
	git pull https://rotskoff@github.com/rotskoff/Haskell-PDB-Utilities.git
