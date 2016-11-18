

all:
	ocamlbuild -no-hygiene NetlistSimul/netlist_simul.byte

clean:
	rm -f netlist_simul.byte
	rm -rf _build/


