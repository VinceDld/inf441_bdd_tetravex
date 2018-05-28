open Formule

module type VARIABLE = sig
	type t
	val val_to_string: t -> string
end

module type BDT = 
	sig
		type bdt
		val formule_to_bdt : Formule.fp -> bdt
	end

module Bdt = functor (V: VARIABLE) ->
	(struct
		type bdt = 
			|Node of bdt * V.t * bdt
			|True
			|False
		let formule_to_bdt f =  True (* TO DO *)
	end:BDT)



module type BDD = 
	sig
		type bdd and node
		val formule_to_bdd : Formule.fp -> bdd
		val file_to_bdd : string -> bdd
		val node_to_string : bdd -> string
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
	end:BDD)