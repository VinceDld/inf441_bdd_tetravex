
module type VARIABLE = sig
	type t
	val val_to_string: t -> string
end

module Formula = functor (V: VARIABLE) ->
struct
	type fp =
		|Var of V.t
		|Cst of bool
		|Not of fp
		|And of fp*fp
		|Or of fp*fp
		|Imp of fp*fp
		|Eq of fp*fp

	let rec toString = function
		|Var a -> " "^(V.val_to_string a)
		|Cst b -> if b then " true" else " false"
		|Not e1 ->" not " ^ toString e1
		|And (e1,e2) -> " (" ^ toString e1 ^ " and" ^ toString e2 ^ ")"
		|Or (e1,e2) -> " (" ^ toString e1 ^ " or" ^ toString e2 ^ ")"
		|Imp (e1,e2) -> " (" ^ toString e1 ^ " =>" ^ toString e2 ^ ")"
		|Eq (e1,e2) -> " (" ^ toString e1 ^ " <=>" ^ toString e2 ^ ")" 

	(* remplace une variable a par un booléen b dans une form une formule propositionnelle, 
	si la variable a n'est dans form renvoie une copie de form non modifié*)
	let rec replace form (a,b) = match form with
		|Var v -> if v = a then Cst b else Var v
		|Cst c -> Cst c
		|Not f -> Not (replace f (a,b))
		|And (f1,f2) -> And (replace f1 (a,b), replace f2 (a,b)) 
		|Or (f1,f2) -> Or (replace f1 (a,b), replace f2 (a,b))
		|Imp (f1,f2) -> Imp (replace f1 (a,b), replace f2 (a,b))
		|Eq (f1,f2) -> Eq (replace f1 (a,b), replace f2 (a,b))

	let rec replace_all form l = match l with
		|[] -> form
		|(a,b)::tl -> replace_all (replace form (a,b)) tl

	let rec ready_for_valuation form = match form with
		|Var v -> false
		|Cst b -> true
		|Not f -> ready_for_valuation f
		|And (f1,f2) |Or (f1,f2) |Imp (f1,f2) |Eq (f1,f2) -> ready_for_valuation f1 && ready_for_valuation f2 

	(* Les différentes opérations logiques sur les booléens *)
	let neg p = not p
	let et p q = p && q
	let ou p q = p || q
	let imp p q = q || (not p)
	let equiv p q = (q || (not p)) || (p || (not q))

	(* Prend une formule sans variable et l'évalue, fail s'il reste une varialbe dans la formule*)
	let rec valuation form =match form with
		|Var v -> failwith "This expression can't be evaluate yet"
		|Cst b -> b
		|Not f -> neg (valuation f)
		|And (f1,f2) -> et (valuation f1) (valuation f2) 
		|Or (f1,f2) -> ou (valuation f1) (valuation f2) 
		|Imp (f1,f2) -> imp (valuation f1) (valuation f2) 
		|Eq (f1,f2) -> equiv (valuation f1) (valuation f2)

	let list_of_var fp = 
		let rec aux f l = match f with
			| Var v -> v::l
			| Cst a -> l
			| Not f0 -> aux f0 l
			| And (f1,f2) |Or (f1,f2) |Imp (f1,f2) |Eq (f1,f2) -> (aux f1 l)@(aux f2 l)
		in aux fp [] 

	exception ExpNotConst
	let only_true_valuation f = valuation (replace_all f (List.map (fun a -> (a,true)) (list_of_var f)))
	let is_constant fp = 
		let rec aux l f = 
		match l with
			|[] -> valuation(f)
			|hd::tl -> if ((aux tl (replace f (hd,true))) != (aux tl (replace f (hd,false)))) then raise (ExpNotConst) else true
		in try aux (list_of_var fp) fp
		with ExpNotConst -> false

end






