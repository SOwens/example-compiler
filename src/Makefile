PACKAGES = str
FLAGS = -bin-annot -g -principal -safe-string -strict-formats -strict-sequence -w +a-42-4

FILES = util tokens sourceAst typeCheck frontEnd constProp unnestExp \
blockStructure shrinkImmediates liveVarAnalysis regAlloc lineariseCfg x86 \
instrSelX86 compileFunction astInterp interp compile

FILES_ML = $(addsuffix .ml,$(FILES))
FILES_MLI = $(addsuffix .mli,$(FILES))
FILES_CMI = $(addsuffix .cmi,$(FILES))
FILES_CMT = $(addsuffix .cmt,$(FILES))
FILES_CMTI = $(addsuffix .cmti,$(FILES))
FILES_CMO = $(addsuffix .cmo,$(FILES))
FILES_CMX = $(addsuffix .cmx,$(FILES))
FILES_O = $(addsuffix .o,$(FILES))

.PHONY : ocb-compile.byte ocb-compile.native ocb-interp.byte ocb-interp.native all

all : ocb-interp.byte ocb-compile.byte

ocb-interp.byte :
	ocamlbuild -use-ocamlfind interp.byte

ocb-interp.native :
	ocamlbuild -use-ocamlfind interp.native

interp.byte : $(FILES_CMO)
	ocamlfind ocamlc -linkpkg -package $(PACKAGES) $(FILES_CMO) -o $@

interp.native : $(FILES_CMX)
	ocamlfind ocamlopt -linkpkg -package $(PACKAGES) $(FILES_CMX) -o $@

ocb-compile.byte :
	ocamlbuild -use-ocamlfind compile.byte

ocb-compile.native :
	ocamlbuild -use-ocamlfind compile.native

compile.byte : $(FILES_CMO)
	ocamlfind ocamlc -linkpkg -package $(PACKAGES) $(FILES_CMO) -o $@

compile.native : $(FILES_CMX)
	ocamlfind ocamlopt -linkpkg -package $(PACKAGES) $(FILES_CMX) -o $@

%.cmi : %.mli
	ocamlfind ocamlc -package $(PACKAGES) $(FLAGS) -o $@ -c $<

%.cmo : %.ml
	ocamlfind ocamlc -package $(PACKAGES) $(FLAGS) -o $@ -c $<

%.cmx : %.ml
	ocamlfind ocamlopt -package $(PACKAGES) $(FLAGS) -o $@ -c $<

.PHONY : depend clean

clean :
	ocamlbuild -clean
	rm -f $(FILES_CMI) $(FILES_CMTI) $(FILES_CMT) $(FILES_CMO) $(FILES_CMX) $(FILES_O) compile.byte compile.native interp.byte interp.native

depend :
	ocamlfind ocamldep $(FILES_ML) $(FILES_MLI) > .depend

include .depend
