
c2py: lexer.cmo parser.cmo c.cmo python.cmo translator.cmo c2py.cmo
	ocamlc -o $@ $^

c2py.cmo: c2py.ml
	ocamlc -c $^

parser.cmo: parser.ml
	ocamlc -c $^

parser.cmi: parser.mli
	ocamlc -c $^

parser.ml: parser.mly c.cmo
	ocamlyacc $<

parser.mli: parser.mly c.cmo
	ocamlyacc $<

lexer.cmo: lexer.ml parser.cmi
	ocamlc -c $<

lexer.ml: lexer.mll
	ocamllex lexer.mll

c.cmo: c.ml
	ocamlc -c $^

python.cmo: python.ml
	ocamlc -c $^

translator.cmo: translator.ml
	ocamlc -c $^

clean:
	rm -rf *.cm[io] lexer.ml parser.ml parser.mli c2py
