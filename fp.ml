
module type VARIABLE = sig
	type t
	val val_to_string: t -> string
end


module Formule = functor (V: VARIABLE) ->
struct

	type fp =
	|Var of V.t
	|T
	|F
	|Not of fp
	|And of fp*fp
	|Or of fp*fp
	|Imp of fp*fp
	|Eq of fp*fp

	let rec toString = function
	|Var a -> V.val_to_string a
	|T -> " true"
	|F -> " false"
	|Not e1 ->" not " ^ toString e1
	|And (e1,e2) -> " (" ^ toString e1 ^ " and" ^ toString e2 ^ ")"
	|Or (e1,e2) -> " (" ^ toString e1 ^ " or" ^ toString e2 ^ ")"
	|Imp (e1,e2) -> " (" ^ toString e1 ^ " =>" ^ toString e2 ^ ")"
	|Eq (e1,e2) -> " (" ^ toString e1 ^ " <=>" ^ toString e2 ^ ")"
	
end




