#IS_TREE = false
#JSON_PATH = forest.json

all:
	@echo compiling treetypes interface...
	ocamlfind ocamlc -c treetypes.mli
	@echo done
	@echo compiling print_goal.ml
	ocamlfind ocamlc -g -thread -linkpkg -package yojson print_goals.ml -o print_goals.native
	@echo printing tree.ml
	@echo compiling tree.ml interface
	ocamlfind ocamlc -c tree.mli
	@echo done
	@echo ... and its binary
	ocamlfind ocamlc -g -thread tree.ml -o tree.native 
	@echo compiling mytermcode interface
	ocamlfind ocamlc -c -linkpkg -package why3 mytermcode.mli
	
	@echo done
	@echo ...and its binary
	ocamlfind ocamlc mytermcode.cmo -g -thread -linkpkg -package str -package unix -package num -package dynlink -package menhirLib -package why3 -package ocamlgraph mytermcode.ml -o mytermcode.native
	@echo done

	@echo compiling get_predictions interface
	ocamlfind ocamlc -c  get_predictions.mli
	@echo and its binary
	ocamlfind ocamlc tree.cmo -g -thread get_predictions.ml -o get_predictions.native
	./get_predictions.native
	@echo done

	@echo compiling make_session interface
	ocamlfind ocamlc -c -linkpkg -package why3 make_session.mli
	@echo ... its binary
	ocamlfind ocamlc make_session.cmo -g -thread -linkpkg -package str -package unix -package num -package dynlink -package menhirLib -package why3 -package ocamlgraph make_session.ml -o make_session.native

	@echo compiling final whyfolio binary
	ocamlfind ocamlc mytermcode.cmo tree.cmo get_predictions.cmo make_session.cmo -g -thread -linkpkg -package str -package unix -package num -package dynlink -package menhirLib -package why3 -package ocamlgraph where4.ml -o where4.native
	@echo done
	@echo moving it to where why3 can find it
	sudo cp ./where4.native /usr/local/bin/where4
	why3 config --detect-provers
