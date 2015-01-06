IVORY_REPO ?= ../ivory
TOWER_REPO ?= ../tower

all:
	cabal build

create-sandbox:
	cabal sandbox init
	cabal sandbox add-source $(IVORY_REPO)/ivory
	cabal sandbox add-source $(IVORY_REPO)/ivory-artifact
	cabal sandbox add-source $(IVORY_REPO)/ivory-backend-c
	cabal sandbox add-source $(IVORY_REPO)/ivory-opts
	cabal sandbox add-source $(IVORY_REPO)/ivory-stdlib
	cabal sandbox add-source $(TOWER_REPO)/tower
	cabal install --dependencies-only

clean:
	-rm -rf dist

clean-sandbox: clean
	-rm -rf .cabal-sandbox
	-rm cabal.sandbox.config
