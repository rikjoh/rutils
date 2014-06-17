
# 


MODULES = ecursed

HRL_INCLUDE_DIRS = ../include

EBIN=../ebin



TARGETS=$(EBIN) $(MODULES:%=$(EBIN)/%.beam)


ERLC_INCLUDES=$(HRL_INCLUDE_DIRS:%=-I %)


all: $(TARGETS)

$(EBIN): 
	mkdir $(EBIN)

$(EBIN)/%.beam: %.erl
	erlc $(ERLC_INCLUDES) -o $(EBIN) $<
