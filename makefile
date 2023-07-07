main: main.ml
	ocamlopt -o pf23 $<

test: test.ml 
	ocamlopt -o $@ $<

clean:
	rm pf23 *.o *.cmi *.cmx test pf23