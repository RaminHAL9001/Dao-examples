.PHONEY: all edit clean

all: Dao-examples.cabal src
	cabal build

Dao-examples.cabal:
	@echo 'Cabal file has been modified';

src:
	@echo 'Source directory has been modified';

edit:
	vim Dao-examples.cabal $$( find . -type f -name '*.hs' )

clean:
	cabal clean

