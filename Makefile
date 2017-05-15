all: evaluator #ast.cmo parser.ml parser.cmi parser.cmo lexer.ml  lexer.cmo evaluator.cmo evaluator

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

evaluator: ast.cmo parser.cmo lexer.cmo evaluator.cmo
	ocamlc -o $@ $^ -g

test: test-assignment test-break test-callcc

test-assignment: test_assignment.my evaluator
	./evaluator < $<

test-break: test_break.my evaluator
	./evaluator < $<

test-callcc: test_callcc.my evaluator
	./evaluator < $<

clean:
	rm -rf evaluator parser.ml lexer.ml *.cmo *.cmi *~ *.mli *.output
