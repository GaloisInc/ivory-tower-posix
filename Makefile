include stack.mk

test: test-tick-gen
test: test-depends-gen
test: test-handlers-gen

%-gen: default
	stack exec -- $@ --src-dir=$* --const-fold
	echo "CFLAGS = -Wall -std=gnu99 -O2 -g -I. -DIVORY_TEST -DTRAVIS_BUILDING" > $*/make.mk
	make -C $*

%-clean:
	rm -rf $*

clean: test-tick-clean
clean: test-depends-clean
clean: test-handlers-clean

TRAVIS_STACK ?= stack --no-terminal --system-ghc --skip-ghc-check

travis-test:
	$(TRAVIS_STACK) build --test --no-run-tests --haddock --no-haddock-deps --pedantic
	make test
