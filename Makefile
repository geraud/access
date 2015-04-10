.PHONY: all bench build clean configure haddock hpc install repl run test

all: install configure build haddock test hpc

build:
	@cabal build --jobs

clean:
	@cabal clean
	if test -d .cabal-sandbox; then cabal sandbox delete; fi
	if test -d .hpc; then rm -r .hpc; fi

configure:
	@cabal configure --enable-tests --ghc-option=-static --disable-shared

haddock:
	@cabal haddock --hyperlink-source
	# dist/doc/html/access/index.html

hpc:
	hpc markup --destdir=tmp dist/hpc/tix/tests/tests.tix

sandbox:
	@cabal sandbox init

install: sandbox
	@cabal install \
		--jobs \
		--only-dependencies \
		--enable-tests \
		--reorder-goals --max-backjumps=200 \
		--force-reinstalls --shadow-installed-packages

repl:
	@cabal repl lib:access

run:
	@cabal run --jobs access

test:
	@cabal test --jobs
	@cabal check
