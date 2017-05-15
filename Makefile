all: compiler #ast.cmo parser.ml parser.cmi parser.cmo lexer.ml  lexer.cmo compiler.cmo compiler

%.cmo:%.ml
	ocamlc -c $^ -g

parser.cmo:parser.ml parser.cmi
	ocamlc -c $< -g

parser.ml: parser.mly
	ocamlyacc -v $<

parser.mli: parser.mly
	ocamlyacc -v $<

parser.cmi: parser.mli
	ocamlc -c $^

lexer.ml: lexer.mll
	ocamllex $^ -o $@

compiler: ast.cmo parser.cmo lexer.cmo compiler.cmo
	ocamlc -o $@ $^ -g

test: test-assignment test-break test-callcc

test-assignment: test_assignment.my compiler
	./compiler < $<

test-break: test_break.my compiler
	./compiler < $<

test-callcc: test_callcc.my compiler
	./compiler < $<

clean:
	rm -rf compiler parser.ml lexer.ml *.cmo *.cmi *~ *.mli *.output
