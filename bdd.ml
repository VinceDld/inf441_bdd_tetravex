open Parser;;
open P;;
open F;;

module Bdt = 
struct
	type bdt = 
		| Node of bdt*char*bdt
		| Leaf of bool
		| ToProcess 

	let node_var n = match n with 
		|Node(left,v,right) -> v
		|_ -> failwith "node_var s'applique seulement au constructeur Node"

	let make_bdt fp = 
		let rec aux l f b= match l with
			| [] -> Leaf(valuation(f))
			| hd::tl -> 
				if (is_constant f) then Leaf ((only_true_valuation f))
				else begin match b with 
					|ToProcess -> Node((aux tl (replace f (hd, false)) ToProcess), hd, (aux tl (replace f (hd, true)) ToProcess))
					|_ -> failwith "bad implementation of make_bdt"
				end
		in aux (list_of_var fp) fp ToProcess

	let bdt_to_string nodeP = 
		let rec aux node = match node with
		| Leaf(b) -> if b then " T "  else " F "
		| Node(left, v, right) -> "{ " ^ (String.make 1 v) ^ " left son :" ^ (aux left) ^ " right son : " ^ (aux right) ^ " }"
		| ToProcess -> failwith "bdt mal construit"
	in aux nodeP 

	exception FindTrue
	let satisfiable_aux bdt = 
		let l = ref [] in
		let flag = ref false in
		let rec aux n =
			match n with
				|ToProcess -> failwith "from satisfiable_path : bdt mal crée"
				|Leaf true  -> flag := true ;raise FindTrue
				|Leaf false -> begin match !l with
					|[] -> failwith "Non satisfiable"
					|hd::tl -> l := tl
				end
				|Node(left, v, right) -> 
				l := (v,true)::(!l);try aux right with FindTrue -> (); 
				if (not !flag) then (l := ((v,false)::(!l));aux left;) else ()

		in aux bdt; (!l)

	let print_a_valuation l = List.iter (fun (a,b) -> print_char a; if b then print_string " @t\n" else print_string " @f\n") l 

(* 	let print_valuation (a,b) = print_char a; if b then print_string " @t\n" else print_string " @f\n" *)

	let satisfiable fp = print_endline"Les variables non affichée peuvent être évalué par @t ou @f";
	print_a_valuation (satisfiable_aux (make_bdt fp)) 

(*  	let satisfiable fp = 
	let rec aux l0 l1= 
		let l2 = List.map fst l1 in 
		match l0 with
		 |[] -> l1
		 |hd :: tl -> if List.mem hd l2 then aux tl l1 else aux tl ((hd,true)::l1)
		in let l = aux (list_of_var fp) (satisfiable_aux (make_bdt fp)) in
		print_a_valuation l
 *)

	let valid fp =
		let rec aux b = match b with
			|Node (left,a,right) -> aux left; aux right;
			|Leaf (a) -> if a then () else failwith "Non valide"
			|ToProcess -> failwith "bdt mal construit"
		in aux (make_bdt fp); exit 0
end

(*  let a = make_bdt (Or(Imp(Var 'p', Var 'q'),And(Var 'r', Var 's')))
	let b = make_bdt (Or(Var 'a', Var 'b'))
	let c = make_bdt (Var 'q') *)


(**************************************
***************************************)
(*  BDD depuis un fichier *)

module B = 
struct
	type node = 
		| Node of node*char*node*int 
		| Leaf of bool

	type bdd = node*((int,node) Hashtbl.t)

	let first_3 (a,b,c) = a
	let second_3 (a,b,c) = b
	let third_3 (a,b,c) = c

	let file_to_hash filename = let file_to_hash_aux filename h =
		let chan = open_in filename in 
			try
				while true; do
					let line = input_line chan in
						let len = String.length line in
						let first = String.index_from line 0 ' ' in
						let second = String.index_from line (first+1) ' ' in
						let third = String.index_from line (second+1) ' ' in 
						let a = (String.sub line (first+1) (second - first-1)) in
						let b = (String.sub line (second+1) (third - second-1)) in
						let c = (String.sub line (third+1) (len - third-1)) in 
					Hashtbl.add h (String.sub line 0 first) (a,b,c)
				done;h
			with
				End_of_file -> close_in chan;
				Hashtbl.add h "@f" ("","","");
				Hashtbl.add h "@t" ("","","");
				h
		in file_to_hash_aux filename (Hashtbl.create 100)

	let file_to_bdd filename = 
		let h = (Hashtbl.create 100) 
	 	in let my_hash = file_to_hash filename in
		let lt = Leaf(true) in let lf = Leaf(false) in
		let rec aux s = match s with
			|"@t" -> if Hashtbl.mem h (-1) then lt else begin Hashtbl.add h (-1) lt; lt end
			|"@f" -> if Hashtbl.mem h (-2) then lf else begin Hashtbl.add h (-2) lf; lf end
			| _ -> if Hashtbl.mem h (int_of_string s) then Hashtbl.find h (int_of_string s) else
				let r = Hashtbl.find my_hash s in 
				let n = Node(aux (third_3 r), (first_3 r).[0] , aux (second_3 r), int_of_string s) in 
				Hashtbl.add h (int_of_string s) n; n
		in (aux "0",h) (* Le root doit avoir 0 comme id *)

	let bdd_to_string nodeP = 
		let rec aux node = match node with
		| Leaf(b) -> if b then " T "  else " F "
		| Node(left, v, right, id) -> "{ " ^ (String.make 1 v) ^ " left son :" ^ (aux left) ^ " right son : " ^ (aux right) ^ " id :"^ (string_of_int id)^ "}"
	in aux nodeP

	let is_valid bdd = let h = snd(bdd) in not (Hashtbl.mem h (-2))

	let is_satisfiable bdd = let h = snd(bdd) in (Hashtbl.mem h (-1))

	exception ExpFindValAux 
	let get_a_true_valuation bdd = 
		let l = ref [] in
		let flag = ref false in
		let rec aux n =
			match n with
				|Leaf true  -> flag := true ;raise ExpFindValAux
				|Leaf false -> begin match !l with
					|[] -> failwith "non satisfiable"
					|hd::tl -> l := tl
				end
				|Node(left, v, right, id) -> 
				l := (v,true)::(!l);try aux right with ExpFindValAux -> (); 
				if (not !flag) then (l := ((v,false)::(!l));aux left;) else ()		
		in aux bdd; !l
end




