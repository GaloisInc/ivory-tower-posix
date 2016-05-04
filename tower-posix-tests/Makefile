include ../stack.mk

IVORYFLAGS ?= --const-fold
TESTS      := test-depends test-tick test-handlers
CLEANS     := $(foreach test,$(TESTS),$(test)-clean)

.PHONY: test clean $(TESTS) $(CLEANS)
test: $(TESTS)
clean: $(CLEANS)

define MKTEST
$(1):
	stack build . --exec '$(1)-gen --src-dir=$(1) $(IVORYFLAGS)'
	echo "CFLAGS = -Wall -std=gnu99 -O2 -g -I. -DIVORY_TEST -DTRAVIS_BUILDING" > $(1)/make.mk
	make -C $(1)
$(1)-clean:
	rm -rf $(1)
endef

$(foreach test,$(TESTS),$(eval $(call MKTEST,$(test))))
