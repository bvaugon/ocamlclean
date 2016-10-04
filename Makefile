###########################################################################
##                                                                       ##
##                              OCamlClean                               ##
##                                                                       ##
##                             Benoit Vaugon                             ##
##                                                                       ##
##    This file is distributed under the terms of the CeCILL license.    ##
##    See file LICENSE-en.                                               ##
##                                                                       ##
###########################################################################

include etc/Makefile.conf

all: config
	$(call compile, src)

config:
	@if [ $(ETC)/Makefile.conf -ot VERSION -o \
             $(ETC)/Makefile.conf -ot configure ]; then \
          echo 'Configuration files are not up to date.' 1>&2; \
	  echo 'Please run `./configure` (with right options).' 1>&2; \
          exit 1; \
	fi

install: all
	mkdir -p "$(BINDIR)"
	mkdir -p "$(MAN1DIR)"
	cp bin/ocamlclean "$(BINDIR)/ocamlclean"
	cp man/ocamlclean.1.gz "$(MAN1DIR)/ocamlclean.1.gz"

uninstall:
	-rm -f "$(BINDIR)/ocamlclean"
	-rm -f "$(MAN1DIR)/ocamlclean.1.gz"

etc/Makefile.conf:
	@echo "You must run ./configure before" 1>&2
	@exit 1

dist: clean
	dist/distgen

clean:
	@rm -f *~ */*~ */*/*~
	$(call clean, src)

.PHONY: all config install uninstall dist clean
