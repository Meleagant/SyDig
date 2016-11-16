
open Netlist_ast
open Netlist
open Scheduler


exception Type_Error_length

(*###################################*)
(* Fonctions de calcul *)
(*###################################*)

let env = Hashtbl.create 30;;
let reg = Stack.create ();;
let attente_ram = Stack.create ();;
let rom = ref [| false |];;
let ram = ref [|false |];;

let bool_to_int b =
	(* bool -> int *)
	if b then 1 else 0;;

let arg_to_int = function
	| VBit b -> bool_to_int b
	| VBitArray t -> 
		let res = ref 0 
		and puiss = ref 1 
		in  begin 
			for i = 0 to (Array.length t)- 1 do
				res := !res + !puiss*(bool_to_int t.(i));
				puiss := !puiss*2;
			done;
			!res; end;;

let calc_binop op x1 x2 = 
match op with 
| Or -> x1 || x2
| And -> x1 && x2
| Xor -> (x1 || x2)&&  not ( x1 && x2)
| Nand -> not (x1 && x2);;

let calc_mux m x1 x2 =
	(* bool -> bool -> bool -> bool *)
	if m then x2 else x1;;

let extract_arg = function
	(* arg -> value *)
	| Aconst valu -> valu
	| Avar id -> Hashtbl.find env id ;;

let eval_eq eq =
let id,expr = eq in
let calc_expr = function
	| Earg x -> extract_arg x
	
	| Ereg id_reg -> begin 
		let arg_id_reg = 
			try Hashtbl.find env id_reg
			with Not_found -> begin
			VBit false; end
		in begin
			Stack.push (id , arg_id_reg ) reg;
			arg_id_reg;
		end ; end

	| Enot x -> begin 
		match extract_arg x with
		| VBit b -> VBit (not b)
		| VBitArray tab -> VBitArray (Array.map (fun x -> not x) tab) 
		end

	| Ebinop (op,x1,x2) ->
		let y1 = extract_arg x1
		and y2 = extract_arg x2 in begin
		match y1,y2 with 
		| VBit b1 , VBit b2 -> VBit (calc_binop op b1 b2)
		| VBitArray t1 , VBitArray t2 
			when (Array.length t1) = (Array.length t2) ->
			let aux i t1i = 
				calc_binop op t1i t2.(i) in
			VBitArray (Array.mapi aux t1)
		| _ ->  raise Type_Error_length
		end

	| Emux (mux,x1,x2) -> begin 
		let m = extract_arg mux
		and y1 = extract_arg x1
		and y2 = extract_arg x2 in
		match m,y1,y2 with
		| VBit bm , VBit b1 , VBit b2->
			VBit (calc_mux bm b1 b2)
		| VBitArray tm , VBitArray t1 , VBitArray t2 
			when ((Array.length tm) = (Array.length t1) &&
				  (Array.length t1) = (Array.length t2) ) ->
			let aux i tmi = 
				calc_mux tmi t1.(i) t2.(i) in
			VBitArray (Array.mapi aux tm)
		| _ ->  raise Type_Error_length
		end
	| Erom ( _ , wds , ra ) ->  
		let ra_int = arg_to_int (extract_arg ra) in
		VBitArray (Array.sub !rom ra_int wds )

	| Eram (ads , wds , ra , we , wa , data ) ->
		let ra_int = arg_to_int (extract_arg ra )
		and we_arg = extract_arg we
		in  
		let res = VBitArray (Array.sub !ram ra_int wds) 
		in begin
		match we_arg with 
		| VBit b when b -> 
			let wa_int = arg_to_int (extract_arg wa)
			in begin
			Stack.push ( wa_int , data ) attente_ram;
				res; end;
		| _ -> res
		end

	| Econcat (x1,x2) -> 
		let y1 = extract_arg x1
		and y2 = extract_arg x2 in begin
		match y1 , y2 with
		| VBit b1 , VBit b2 ->
			VBitArray [| b1 ; b2 |]
		| VBit b1 , VBitArray t2 ->
			VBitArray ( Array.append [| b1|] t2 )
		| VBitArray t1 , VBit b2 ->
			VBitArray ( Array.append t1 [|b2|] )
		| VBitArray t1 , VBitArray t2 ->
			VBitArray ( Array.append t1 t2)
		end

	| Eslice (i1,i2,a) -> begin
		let x = extract_arg a in
		match x with
		| VBitArray t -> 
			VBitArray ( Array.sub t i1 (i2 - i1 + 1))
		| VBit b  ->  VBit b
		end

	| Eselect (i,a) -> 
		let x = extract_arg a in
		match x with
		| VBitArray t -> 
			VBit t.(i)
		| VBit b -> VBit b
in
	let res = calc_expr expr in
	Hashtbl.replace env id res;;
			


(*###################################*)
(* fonction de gestion des outputs *)
(*###################################*)

let bool_of_char c =
	c = '1' ;;

let init_var p =
	(* ident -> unit *)
	let init_aux v =
		print_string (v^" = ? ");
		let l = read_line () in 
		if String.length l = 1 
			then Hashtbl.replace env v (VBit (l = "1"))
			else 
				let res = Array.make (String.length l) false
				in begin
				for i = 0 to (String.length l) -1 do
					res.(i) <- bool_of_char l.[i]
				done; 
				Hashtbl.replace  env v (VBitArray res); 
				end;
	in List.iter init_aux p.p_inputs;;

let import_rom ()  = 
	begin
	print_string "Importation de la ROM \n";
	let l = read_line () in 
	let n = String.length l 
	in begin
		rom := Array.make n false;
		for i = 0 to n-1 do
			!rom.(i) <- bool_of_char l.[i]
		done; 
		end;
	end;;

let import_ram () = 
	begin
	print_string "Importation de la RAM \n";
	let l = read_line () in 
	let n = String.length l 
	in begin
		ram := Array.make n false;
		for i = 0 to n-1 do
			!ram.(i) <- bool_of_char l.[i]
		done; 
		end;
	end;;

let string_of_bool b =
	if b then "1" else "0";;

let string_of_value = function
	| VBit b -> string_of_bool b
	| VBitArray t -> 
		let aux s b = 
			s^(string_of_bool b) in
		Array.fold_left aux "" t;;

let print_outputs env p =
	let print_output id =
	let v = Hashtbl.find env id in 
		let s = string_of_value v in
		print_string ("=>> "^id^" = "^s^" \n")
	in List.iter print_output p.p_outputs ;;

(*###################################*)
(* Fonction principale *)
(*###################################*)

 
let main () =
let filename = ref ""
and n_iter = ref 0
in begin
	print_string "fichier à traiter ? \n";
	filename := read_line ();
	print_string "Nombre d'itération \n";
	n_iter := read_int ();
	import_ram () ;
	import_rom () ; 
	let prog = schedule (read_file !filename) 
	in begin
	let out_name =(Filename.chop_suffix (!filename) ".net") ^"_sch.net" in
		let out = open_out out_name in
	print_program out prog;
	for i = 1 to (!n_iter) do 
		print_string ("Step "^(string_of_int i)^" \n");
		init_var prog;
		List.iter eval_eq prog.p_eqs;
		print_outputs env prog;
		let aux ( id , arg ) = 
			Hashtbl.replace env id arg 
		and aux2 ( wa_int , data) = 
			let data_arg = extract_arg data in
			match data_arg with
			| VBit b_data -> 
				!ram.(wa_int) <- b_data
			| VBitArray t -> 
				let aux_3 i x =
					!ram.(wa_int + 1) <- x
				in 
				Array.iteri aux_3 t; 
		in begin			
		Stack.iter aux reg;
		Stack.clear reg;
		Stack.iter aux2 attente_ram ;
		Stack.clear attente_ram; end 
	done;
end;
end;;

main ();






























































