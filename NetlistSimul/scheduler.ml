open Netlist_ast
open Graph

exception Combinational_cycle

let read_exp eq = 
let res = ref [] in
let add x = 
	if not (List.mem x !res)
		then res := x::(!res) in
let read_arg = function
	Avar x -> add x	
	| _ -> ()
and (id,expr) = eq in begin
(match expr with
	Earg x | Enot x -> read_arg x
	| Ereg x -> ()
	| Ebinop (op,x1,x2) -> begin
		read_arg x1;
		read_arg x2; end
	| Emux (x1,x2,x3) -> begin
		read_arg x1; 
		read_arg x2; 
		read_arg x3;  end
	| Erom (a,l,x) -> 
		read_arg x;
	| Eram (a,l,x1,x2,x3,x4) -> begin
		read_arg x1;
		read_arg x2;
		read_arg x3; end
	| Econcat (x1,x2) -> begin
		read_arg x1;
		read_arg x2; end
	| Eslice (i1,i2,a) -> read_arg a
	| Eselect (i,a) -> read_arg a );
!res; end
	

let schedule p =
let g = mk_graph () in
let add_nd_g x = add_node g x 

and traite_eq eq =
    (* On rajoute des arêtes eq° par eq°*)
	let id,exp = eq in
	let add_ed_g id2 = 
		add_edge g id2 id 
	in 
	List.iter add_ed_g (read_exp eq) 

and pas_input x =
	(* renvoie true ssi x n'est pas une input *)
	(* i.e. est une var/output                *)
	not (List.exists (fun t->x=t) p.p_inputs)

and ret_couple id =
	(* A une variable/outpout, on associe l'équation qui la calcule *)
	(id, List.assoc id p.p_eqs )

	in begin
	(*####    construct° du graphe    ####*)
	List.iter add_nd_g p.p_inputs;             (* ajout des inputs *)
	List.iter add_nd_g (List.map fst p.p_eqs); (* ajout des outputs/variables *)
	List.iter traite_eq p.p_eqs;               (* On rajoute les arêtes *)
	(*####    Travail sur le graphe   ####*)
	if has_cycle g then raise  Combinational_cycle;
	let calculable = ref p.p_inputs 
	and en_attente = ref p.p_eqs
	and cont = ref true 
		in begin 
		while !cont do
		match !en_attente with
		| [] -> cont := false
		| t::q -> 
			if (List.for_all (fun x -> List.mem x !calculable) (read_exp t))
			(* Ce test détermine si chaque variable dans l'éqution est
			déjà dans calculable *)
			then begin 
				calculable := !calculable @ [fst t];
				en_attente := q;
			end
			else en_attente := q @ [t];
		done;
		
	 	{ p_eqs = List.map ret_couple (List.filter pas_input !calculable) ;
	  	p_inputs = p.p_inputs ;
	  	p_outputs = p.p_outputs ;
	  	p_vars = p.p_vars };
		end;
	end 

	



