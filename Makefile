

all:
	ocamlbuild -no-hygiene NetlistSimul/netlist_simul.byte

clean:
	#by default, reset every .byte file
	cd NetlistSimul/
	rm netlist_simul.byte
	rm -r _build/


