IVORY_REPO ?= ../ivory
TOWER_REPO ?= ../tower
include Makefile.sandbox

test: test-tick-gen
test: test-depends-gen
test: test-handlers-gen

%-gen:
	cabal run $@ -- --src-dir=$* --const-fold
	make -C $*

%-clean:
	rm -rf $*

clean: test-tick-clean
clean: test-depends-clean
clean: test-handlers-clean
