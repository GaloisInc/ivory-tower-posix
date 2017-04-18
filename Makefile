include stack.mk

test: test-tick-gen
test: test-depends-gen
test: test-handlers-gen

%-gen: default
	stack exec -- $@ --src-dir=$* --const-fold
	make -C $*

%-clean:
	rm -rf $*

clean: test-tick-clean
clean: test-depends-clean
clean: test-handlers-clean

TRAVIS_STACK ?= stack --no-terminal

travis-test:
	$(TRAVIS_STACK) setup
	$(TRAVIS_STACK) build --test --no-run-tests --haddock --no-haddock-deps --pedantic
	make test
