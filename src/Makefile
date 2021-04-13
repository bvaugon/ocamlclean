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
	$(OCAMLBUILD) -cflag -g $(BUILD)

config.ml: ../VERSION
	sed -ne '1s/.*/let version = "&"/p' $< > $@

clean:
	@$(OCAMLBUILD) -clean
	@rm -f *~ config.ml $(TARG)

.PHONY: ocamlclean clean
