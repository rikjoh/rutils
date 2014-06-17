

TOP=$(shell git rev-parse --show-toplevel)
-include $(TOP)/make/make.variables

EBIN=../ebin


MODULES = simple_read_cfg


TARGETS = $(MODULES:%=$(EBIN)/%.beam)



all: $(TARGETS)

include $(TOP)/make/make.rules
