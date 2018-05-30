open Fp;;

module type VARIABLE = sig
	type t
	val val_to_string: t -> string
end

module type BDT = 
	sig
		type bdt
	end

module Bdt = functor (V: VARIABLE) ->
	(struct
		type node  = 
			|Node of node * V.t * node 
			|True 
			|False 
		and bdt = 
			|Root of node * ((bool list) ref)
			|Empty

		let build_bdt f b = match b with
			|Empty ->  match f with
				|Var v -> Root(Node(False, v, True),(false::true::[]) ref)
				|Cst b -> Cst b
				|Not f -> Not (replace f (a,b))
				|And (f1,f2) -> And (replace f1 (a,b), replace f2 (a,b)) 
				|Or (f1,f2) -> Or (replace f1 (a,b), replace f2 (a,b))
				|Imp (f1,f2) -> Imp (replace f1 (a,b), replace f2 (a,b))
				|Eq (f1,f2) -> Eq (replace f1 (a,b), replace f2 (a,b))



(* 		let get_valuation bdt = 
			let rec aux b l = match b with
				|Node (n) -> if n.t = b then aux n ( (n.var,true)::l )
					else aux n ( (n.var,false)::l )
				|True (f) -> if f.t = b then aux f ( (f.var,true)::l )
					else aux n ( (b.var,false)::l ) 
				|False (f) -> if f.t = b then aux f ( (f.var,true)::l )
					else aux n ( (b.var,false)::l )
				|Root (t,var,f) -> if t = b then (var,true)::l
					else (var,false)::l
			in aux bdt [] *)

		let formule_to_bdt f =  ()  (* TO DO *)
	end:BDT)



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