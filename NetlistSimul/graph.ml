exception Cyclextypei
type mark = NotVisited | InProgress | Visited

type 'a graph =
    { mutable g_nodes : 'a node list }
and 'a node = {    
  n_label : 'a;    
  mutable n_mark : mark;  
  mutable n_link_to : 'a node list; 
  mutable n_linked_by : 'a node list;
}

let mk_graph () = { g_nodes = [] }

let add_node g x =
  let n = { n_label = x; n_mark = NotVisited; n_link_to = []; n_linked_by = [] } in
  g.g_nodes <- n::g.g_nodes

let node_for_label g x =
  List.find (fun n -> n.n_label = x) g.g_nodes

let add_edge g id1 id2 =
  let n1 = node_for_label g id1 in
  let n2 = node_for_label g id2 in
  n1.n_link_to <- n2::n1.n_link_to;
  n2.n_linked_by <- n1::n2.n_linked_by

let clear_marks g =
  List.iter (fun n -> n.n_mark <- NotVisited) g.g_nodes

let find_roots g =
  List.filter (fun n -> n.n_linked_by = []) g.g_nodes

let has_cycle g = 
  let en_attente = find_roots g 
  and res = ref false in 
    let rec aux n = 
      match n.n_mark with
	NotVisited -> 
	  n.n_mark <- Visited;
	  List.iter aux n.n_linked_by;
	  n.n_mark <- NotVisited;
	|_ -> res := true in begin
    if en_attente = [] 
		then res := true 
		else List.iter aux en_attente;
    !res; end


let topological g =
	(* renvoie les sommets du garphe en partant des *) 
	(* racines et en terminant par les feuilles     *)
	if has_cycle g 
    then failwith "ce garphe a un cycle"
    else 
		let en_attente = ref (find_roots g)  
    	and res = ref []
    	in begin
		clear_marks g;
		while !en_attente <> [] do
			match !en_attente with 
			t::q ->  
			if t.n_mark = NotVisited 
	  		then begin
	    		res := !res@[t.n_label] ; 
	    		t.n_mark <- Visited ; 
	    		en_attente := q@t.n_link_to;end
	  		else 
				en_attente := List.tl (!en_attente)
		done;
		!res;end
    
	 




