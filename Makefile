include stack.mk

TRAVIS_STACK ?= stack --no-terminal --system-ghc --skip-ghc-check

travis-test:
	$(TRAVIS_STACK) build --test --no-run-tests --haddock --no-haddock-deps --pedantic
	make test -C tower-posix-tests
