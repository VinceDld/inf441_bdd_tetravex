
module type VARIABLE = sig
	type a
	val valToString: a -> string
end


module Formule = functor (V: VARIABLE) ->
struct

	type fp =
	|Var of V.a
	|T
	|F
	|Not of fp
	|And of fp*fp
	|Or of fp*fp
	|Imp of fp*fp
	|Eq of fp*fp

	let rec toString = function
	|Var a -> V.valToString a
	|T -> " true"
	|F -> " false"
	|Not e1 ->" not " ^ toString e1
	|And (e1,e2) -> " (" ^ toString e1 ^ " and" ^ toString e2 ^ ")"
	|Or (e1,e2) -> " (" ^ toString e1 ^ " or" ^ toString e2 ^ ")"
	|Imp (e1,e2) -> " (" ^ toString e1 ^ " =>" ^ toString e2 ^ ")"
	|Eq (e1,e2) -> " (" ^ toString e1 ^ " <=>" ^ toString e2 ^ ")"
	
end




