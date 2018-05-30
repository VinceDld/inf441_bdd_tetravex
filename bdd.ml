open Parser;;
open P;;
open F;;

type node = 
	| Node of node*char*node 
	| Leaf of bool
	| ToProcess 

	let node_var n = match n with 
		|Node(left,v,right) -> v
		|_ -> failwith "node_var s'applique seulement au constructeur Node"

	let make_bdt fp = 
		let rec aux l f b= match l with
			| [] -> Leaf(valuation(f))
			| hd::tl -> let a = is_constant f in
				if a then Leaf ((only_true_valuation f))
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

	let a = make_bdt (Or(Imp(Var 'p', Var 'q'),And(Var 'r', Var 's')))
	let b = make_bdt (Or(Var 'a', Var 'b'))
	let _ = print_string ((bdt_to_string a)); 


(* CODE PERMETTANT DE CREER UN BDT SIMPLE AVEC 0 OPTIMISATION*)
(* type node = 
	| Node of node*char*node 
	| Leaf of bool option ref

	let make_bdt fp = 
		let rec aux1 l = match l with
			| [] -> Leaf(ref None)
			| hd::tl -> Node(aux1 tl, hd, aux1 tl)
		in let b0 = aux1 (list_of_var fp) 
		in let rec aux2 f b = match b with
			|Leaf(a) -> begin match !a with
				|None -> a := (Some(valuation(f))); ();
				|Some(k) -> failwith "This leaf has already been evaluated"
			end
			|Node(left, v, right) -> begin 
				aux2 (replace f (v, false)) left;
				aux2 (replace f (v, true)) right;
			end
		in aux2 fp (b0); b0

	let leaf_to_bool a = match !a with
		| None -> failwith "A leaf have not been evaluated"
		| Some(b) -> b

	(* Parcours prefixe *)
	let bdt_to_string nodeP = 
		let rec aux node s = match node with
		| Leaf(b) -> if leaf_to_bool (b) then s^" T " else s^" F "
		| Node(left, v, right) -> aux right ((aux left s)^(String.make 1 v))
	in aux nodeP "" 

	let a = make_bdt (Or(Imp(Var 'p', Var 'q'),And(Var 's', Var 's')))
	let b = make_bdt (Or(Var 'a', Var 'b'))
	let _ = print_string ("Arbre prefixe : "^(bdt_to_string a));  *)


(* module type BDD = 
	sig
		type bdd and node
		val formule_to_bdd : Formule.fp -> bdd
		val file_to_bdd : string -> bdd
		val node_to_string : bdd -> string
		val display : bdd -> unit
	end

module Bdd = functor (V: VARIABLE) ->
	(struct
		type bdd = 
			|Node of node
			|True
			|False
		and node = {id : int ; var : V.t ; t : bdd ; f : bdd}

		let formule_to_bdd f =  True (* TO DO *)
		let file_to_bdd filename = False (* TO DO *)
		let node_to_string b = match b with
			|True | False -> ""
			|Node n -> match n.t with 
				|True -> begin match n.f with
					|True -> string_of_int n.id ^ " " ^ V.val_to_string n.var ^ " " ^ "@t" ^ " " ^ "@t" ^ "\n"
					|False -> string_of_int n.id ^ " " ^ V.val_to_string n.var ^ " " ^ "@t" ^ " " ^ "@f" ^ "\n"
					|Node nf -> string_of_int n.id ^ " " ^ V.val_to_string n.var ^ " " ^ "@t" ^ " " ^ string_of_int nf.id ^ "\n"
				end
				|False -> begin match n.f with
					|True -> string_of_int n.id ^ " " ^ V.val_to_string n.var ^ " " ^ "@f" ^ " " ^ "@t" ^ "\n"
					|False -> string_of_int n.id ^ " " ^ V.val_to_string n.var ^ " " ^ "@f" ^ " " ^ "@f" ^ "\n"
					|Node nf -> string_of_int n.id ^ " " ^ V.val_to_string n.var ^ " " ^ "@f" ^ " " ^ string_of_int nf.id ^ "\n"
				end
				|Node nt -> begin match n.f with
					|True -> string_of_int n.id ^ " " ^ V.val_to_string n.var ^ " " ^ string_of_int nt.id ^ " " ^ "@t" ^ "\n"
					|False -> string_of_int n.id ^ " " ^ V.val_to_string n.var ^ " " ^ string_of_int nt.id ^ " " ^ "@f" ^ "\n"
					|Node nf -> string_of_int n.id ^ " " ^ V.val_to_string n.var ^ " " ^ string_of_int nt.id ^ " " ^ string_of_int nf.id ^ "\n"
				end
		let display b = ()
	end:BDD) *)