# Copyright 2015 Hakan Mattsson
#
# See the file "LICENSE" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

include ./include.mk
SUBDIRS = src

all: Makefile lib/yaws/done
	@for d in $(SUBDIRS); do         \
	  if test ! -d $$d ; then        \
	    echo "=== Skipping subdir $$d" ; \
	  else                   \
	    (cd $$d && $(MAKE) $@) ; \
	  fi ;                        \
	done

lib/yaws/done:
	(cd lib && git clone https://github.com/klacke/yaws.git)
	(cd lib/yaws && autoreconf -fi && ./configure && make)
	touch lib/yaws/done

config_clean:
	$(MAKE) clean
	-rm -rf lib/yaws
	-rm -rf configure include.mk autom4te.cache config.status config.log *~

clean: Makefile
	rm -rf lib/yaws/done lib/yaws
	@for d in $(SUBDIRS); do         \
	  if test ! -d $$d ; then        \
	    echo "=== Skipping subdir $$d" ; \
	  else                   \
	    (cd $$d && $(MAKE) $@) ; \
	  fi ;                        \
	done
