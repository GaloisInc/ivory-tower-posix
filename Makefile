IVORY_REPO ?= ../ivory
TOWER_REPO ?= ../tower
include Makefile.sandbox

test: test-handlers-gen

%-gen:
	cabal run $@ -- --src-dir=$* --const-fold
	make -C $*

%-clean:
	rm -rf $*

clean: test-handlers-clean
