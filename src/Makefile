###########################################################################
##                                                                       ##
##                              OCamlClean                               ##
##                                                                       ##
##                             Benoit Vaugon                             ##
##                                                                       ##
##    This file is distributed under the terms of the CeCILL license.    ##
##    See file ../LICENSE-en.                                            ##
##                                                                       ##
###########################################################################

include ../etc/Makefile.conf

TARG = $(BIN)/ocamlclean
SRCS = $(wildcard *.ml)
BUILD = main.native

ocamlclean: $(TARG)

$(TARG): _build/$(BUILD)
	cp $< $@

_build/$(BUILD): $(SRCS) config.ml
	$(OCAMLBUILD) -cflags -I,+../obytelib,-g -lflags -I,+../obytelib,obytelib.cmxa $(BUILD)

config.ml: $(ETC)/config.ml
	cp $< $@

clean:
	@$(OCAMLBUILD) -clean
	@rm -f *~ config.ml $(TARG)

.PHONY: ocamlclean clean
