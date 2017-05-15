compiler: ast.cmo parser.cmi parser.cmo lexer.cmo compiler.cmo
	ocamlc -o compiler lexer.cmo ast.cmo parser.cmo compiler.cmo -g

.SUFFIXES: .mll .mly .mli .ml .cmi .cmo .cmx

.mll.mli:
	ocamllex $<

.mll.ml:
	ocamllex $<

.mly.mli:
	ocamlyacc $<

.mly.ml:
	ocamlyacc $<

.mli.cmi:
	ocamlc -c $^ -g

.ml.cmo:
	ocamlc -c $^ -g

test: test.my
	./compiler < $^

clean:
	rm -rf *.cm*  *.mli compiler parser.ml *~
