main: main.ml
	ocamlopt -o pf23 $<

%: %.ml 
	ocamlopt -o $@ $<

clean:
	rm pf23 *.o *.cmi *.cmx test pf23 backup