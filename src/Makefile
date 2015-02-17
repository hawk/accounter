# Copyright 2015 Hakan Mattsson
#
# See the file "LICENSE" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

include ../include.mk
include ../vsn.mk
VSN=$(ACCOUNTER_VSN)

ERL_MODULES = \
	accounter \
	accounter_app \
	accounter_sup \
	accounter_vsn

ERL_FILES=$(ERL_MODULES:%=%.erl)
EBIN_FILES=$(ERL_MODULES:%=../ebin/%.$(EMULATOR)) ../ebin/accounter.app
APP_DIR=$(shell dirname $(PWD))

all: $(EBIN_FILES)

$(EBIN_FILES): ../include/accounter.hrl

clean:
	rm -f $(EBIN_FILES) erl_crash.dump core* *~

install:
	mkdir -p $(DESTDIR)
	$(APP_DIR)/bin/accounter --install $(DESTDIR)
