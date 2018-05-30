
module Formula = 
struct


	type fp =
		|Cst of bool
		|Var of char
		|Not of fp
		|And of fp*fp
		|Or of fp*fp
		|Imp of fp*fp
		|Eq of fp*fp

type expr = 
	| StringSplit of string * char * string
	| NotOperation of string
	| Single of string

let rec stringToArray s = let tab = ref [] in 
	for i = 0 to String.length s - 1 do
		tab := s.[i]::!tab;
	done;
	List.rev !tab

let split s =
	if String.length s = 1 then Single(s) 
	else if (s.[0] != '(' && s.[0] != 'N') then 
		if s.[2] != '(' then StringSplit(String.sub s 0 1, s.[1], String.sub s 2 (String.length s - 2))
			else StringSplit(String.sub s 0 1, s.[1], String.sub s 3 (String.length s - 4))
	else	
	let rec aux index cpt = (* regarde à partir de index jusqu'a trouver ) qd cpt = 1 *)
		if s.[index] = 'N' then 
			if s.[index + 1] = '(' then NotOperation(String.sub s (index + 2) (String.length s - (index + 3)))
				else NotOperation(String.sub s (index + 1) (String.length s - (index + 1)))
		else
		let i = ref index in
		while (!i < String.length s && s.[!i] != '(' && s.[!i] != ')') do
			i := !i + 1
		done;
		if (!i = String.length s) then 
			match cpt with
			| 0 -> Single s
			| _ -> failwith "Parentheses pas fermees"
		else
		match s.[!i] with 
		| '(' -> aux (!i+1) (cpt + 1)
		| ')' -> 
			begin
				match cpt with
				| 0 -> failwith "Parentheses pas bien fermees"
				| 1 -> if s.[!i+2]='(' then 
					StringSplit(String.sub s 1 (!i - 1), s.[!i+1], String.sub s (!i+3) (String.length s - (!i+4)))
				else 
					StringSplit(String.sub s 1 (!i - 1), s.[!i+1], String.sub s (!i+2) (String.length s - (!i+2)))
				| _ -> aux (!i+2) (cpt - 1)
			end
		| _ -> failwith "Erreur dans la fonction split sur le while"
	in aux 0 0

let rec builder s =
	match split s with
	| NotOperation s -> Not(builder s)
	| Single s -> 
		begin
			match stringToArray s with 
			| c1::c2::c3::[] -> begin
				match c2 with
				| 'A' -> And(Var(c1), Var(c3))
				| 'O' -> Or(Var(c1),Var(c3))
				| 'I' -> Imp(Var(c1),Var(c3))
				| 'E' -> Eq(Var(c1),Var(c3))
				| _ -> failwith "Operation attendue"
			end
			| c1::c2::[] -> if (c1 = 'N') then Not(Var(c2)) else failwith "Mauvaise negation"
			| c1::[] -> begin
				match c1 with 
				| 'F' -> Cst(false)
				| 'T' -> Cst(true)
				| _ -> Var(c1)
			end
			| _ -> failwith "Mauvaise longueur du Single"
		end
	| StringSplit (s1, c, s2) -> 
		begin
			match c with
			| 'A' -> And(builder s1, builder s2)
			| 'O' -> Or(builder s1, builder s2)
			| 'I' -> Imp(builder s1, builder s2)
			| 'E' -> Eq(builder s1, builder s2)
			| _ -> failwith "Operation attendue"
		end

 (* Debug de split *)
let _ = assert (stringToArray "azert" = ['a'; 'z'; 'e'; 'r'; 't'])
let _ = assert (split "(1)A(2)" = StringSplit("1", 'A', "2"))
let _ = assert (split "(1)A2" = StringSplit("1", 'A', "2"))
let _ = assert (split "1A2" = StringSplit("1", 'A', "2"))
let _ = assert (split "1A(2)" = StringSplit("1", 'A', "2"))
let _ = assert (split "N(2)" = NotOperation("2"))
let _ = assert (split "N2" = NotOperation("2"))
let _ = assert (split "a" = Single("a"))
let _ = assert (builder "a" = Var('a'))
let _ = assert (builder "T" = Cst(true))
let _ = assert (builder "aAb" = And(Var('a'), Var('b')))
let _ = assert (builder "(aAb)Oc" = Or(And(Var('a'), Var('b')),Var('c')))
let _ = assert (builder "aA(bOc)" = And(Var('a'), Or(Var('b'),Var('c'))))
let _ = assert (builder "Na" = Not(Var('a')))
let _ = assert (builder "N(aA(bOc))" = Not(And(Var('a'), Or(Var('b'),Var('c')))))

let authorised_caracters = ["=>" ; "<=>" ; "~" ; "and" ; "or" ; "(" ; ")" ; "true" ; "false" ; "#" ]
let lexer s = Genlex.make_lexer authorised_caracters (Stream.of_string s)
let parser s = 
	let q = lexer s in
	let rec aux acc =
		match Stream.next q with
			|Genlex.Kwd "=>" -> aux (acc^"I")
			|Genlex.Kwd "<=>" -> aux (acc^"E")
			|Genlex.Kwd "~" -> aux (acc^"N")
			|Genlex.Kwd "and" -> aux (acc^"A")
			|Genlex.Kwd "or" -> aux (acc^"O")
			|Genlex.Kwd "(" -> aux (acc^"(")
			|Genlex.Kwd ")" -> aux (acc^")")
			|Genlex.Kwd "true" -> aux (acc^"T")
			|Genlex.Kwd "false" -> aux (acc^"F")
			|Genlex.Kwd "#" -> acc
			|Genlex.Ident c -> aux (acc^c)
			|_ -> failwith "forbidden characters"
	in aux ""

let input_formula () =
	print_endline "Enter a prositionnal formula :";
	(read_line ()) ^ "#";;

let rec toString = function
	|Var a -> " " ^ String.make 1 a
	|Cst a -> if a then "true" else "false"
	|Not e1 ->" not" ^ toString e1
	|And (e1,e2) -> "(" ^ toString e1 ^ " and" ^ toString e2 ^ ")"
	|Or (e1,e2) -> "(" ^ toString e1 ^ " or" ^ toString e2 ^ ")"
	|Imp (e1,e2) -> "(" ^ toString e1 ^ " =>" ^ toString e2 ^ ")"
	|Eq (e1,e2) -> "(" ^ toString e1 ^ " <=>" ^ toString e2 ^ ")"

let s = input_formula ()
let clean_formula = parser s
let a = builder clean_formula
let _ =print_endline (toString a)

	(*let neg p = not p
	let et p q = p && q
	let ou p q = p || q
	let imp p q = q || (not p)
	let equiv p q = (q || (not p)) || (p || (not q))*)

	(*let rec toString = function
	|Var a -> V.val_to_string a
	|T -> " true"
	|F -> " false"
	|Not e1 ->" not " ^ toString e1
	|And (e1,e2) -> " (" ^ toString e1 ^ " and" ^ toString e2 ^ ")"
	|Or (e1,e2) -> " (" ^ toString e1 ^ " or" ^ toString e2 ^ ")"
	|Imp (e1,e2) -> " (" ^ toString e1 ^ " =>" ^ toString e2 ^ ")"
	|Eq (e1,e2) -> " (" ^ toString e1 ^ " <=>" ^ toString e2 ^ ")" *)
(* 
	let authorised_caracters = ["=>" ; "<=>" ; "~" ; "and" ; "or" ; "(" ; ")"]
	let lexer s = Genlex.make_lexer authorised_caracters (Stream.of_string s)
	let rec parser p = let s =lexer st in
		match st.next with
			|Int 0 -> True
			|Kwd "("  *)

	(*let rec parser =function
		| [< ’Int 0 ; (parse_bin (False)) n >] −> n
		| [< ’Int 1 ; (parse_bin (True)) n >] −> n
		| [< ’Ident c ; (parse_bin (Var (nth_char c 0))) n >] −> n
		| [< ’Kwd "(" ; parse n1 ; ’Kwd ")" ; (parse_bin n1) n2 >] −> n2
		| [< ’Kwd "~" ; parse_uni n1 ; (parse_bin (Not (n1))) n2 >] −> n2
		and
		parse_uni = function
		| [< ’Int 0 >] −> False
		| [< ’Int 1 >] −> True
		| [< ’Ident c >] −> Var (nth_char c 0)
		| [< ’Kwd "(" ; parse n ; ’Kwd ")" >] −> n
		and
		parse_bin n1 = function
		| [< ’Kwd "and" ; parse n2 >] −> And (n1, n2)
		| [< ’Kwd "or" ; parse n2 >] −> Or (n1, n2)
		| [< ’Kwd "=>" ; parse n2 >] −> Imp (n1, n2)
		| [< ’Kwd "<=>" ; parse n2 >] −> Eq (n1, n2)
		| [< >] −> n1 *)
end



(*module type VARIABLE = sig
	type t
	val val_to_string: t -> string
end


module Formula = functor (V: VARIABLE) ->
struct


	type fp =
		|Var of V.t
		|True
		|False
		|Not of fp
		|And of fp*fp
		|Or of fp*fp
		|Imp of fp*fp
		|Eq of fp*fp


	(*let neg p = not p
	let et p q = p && q
	let ou p q = p || q
	let imp p q = q || (not p)
	let equiv p q = (q || (not p)) || (p || (not q))*)

	let rec toString = function
	|Var a -> V.val_to_string a
	|T -> " true"
	|F -> " false"
	|Not e1 ->" not " ^ toString e1
	|And (e1,e2) -> " (" ^ toString e1 ^ " and" ^ toString e2 ^ ")"
	|Or (e1,e2) -> " (" ^ toString e1 ^ " or" ^ toString e2 ^ ")"
	|Imp (e1,e2) -> " (" ^ toString e1 ^ " =>" ^ toString e2 ^ ")"
	|Eq (e1,e2) -> " (" ^ toString e1 ^ " <=>" ^ toString e2 ^ ")" 

	let authorised_caracters = ["=>" ; "<=>" ; "~" ; "and" ; "or" ; "(" ; ")"]
	let lexer s = Genlex.make_lexer authorised_caracters (Stream.of_string s)
	let rec parser =function
		| [< ’Int 0 ; (parse_bin (False)) n >] −> n
		| [< ’Int 1 ; (parse_bin (True)) n >] −> n
		| [< ’Ident c ; (parse_bin (Var (nth_char c 0))) n >] −> n
		| [< ’Kwd "(" ; parse n1 ; ’Kwd ")" ; (parse_bin n1) n2 >] −> n2
		| [< ’Kwd "~" ; parse_uni n1 ; (parse_bin (Not (n1))) n2 >] −> n2
		and
		parse_uni = function
		| [< ’Int 0 >] −> False
		| [< ’Int 1 >] −> True
		| [< ’Ident c >] −> Var (nth_char c 0)
		| [< ’Kwd "(" ; parse n ; ’Kwd ")" >] −> n
		and
		parse_bin n1 = function
		| [< ’Kwd "and" ; parse n2 >] −> And (n1, n2)
		| [< ’Kwd "or" ; parse n2 >] −> Or (n1, n2)
		| [< ’Kwd "=>" ; parse n2 >] −> Imp (n1, n2)
		| [< ’Kwd "<=>" ; parse n2 >] −> Eq (n1, n2)
		| [< >] −> n1 
end
*)


