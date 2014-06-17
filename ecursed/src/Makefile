
# 
TOP=$(shell git rev-parse --show-toplevel)
-include $(TOP)/make/make.variables


EBIN=../ebin

MODULES = ecursed

HRL_INCLUDE_DIRS = ../include



TARGETS=$(MODULES:%=$(EBIN)/%.beam)


all: $(TARGETS)

include $(TOP)/make/make.rules

