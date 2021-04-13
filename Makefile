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

DUNE=dune

all:
	$(DUNE) build @install

install: all
	$(DUNE) install

uninstall:
	$(DUNE) uninstall

dist: clean
	dist/distgen

clean:
	$(DUNE) clean

.PHONY: all install uninstall dist clean
