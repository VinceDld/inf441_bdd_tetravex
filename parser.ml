open Fp;;

module Var_Char = struct
	type t = char
	let val_to_string a = String.make 1 a
end

module F = Formula(Var_Char)

(************ PARSER ************)
module P = 
struct
	(* Type pour représenter les différents types d'opérations, constant booléan ou variable, opération unaira (not) ou opération binaire*)
	type expr = 
		| StringSplit of string * char * string
		| NotOperation of string
		| Single of string

	(* Fonction auxilaire qui convertit une string en list de char*)
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
	| NotOperation s -> F.Not(builder s)
	| Single s -> 
		begin
			match stringToArray s with 
			| c1::c2::c3::[] -> begin
				match c2 with
				| 'A' -> F.And(F.Var(c1), F.Var(c3))
				| 'O' -> F.Or(F.Var(c1),F.Var(c3))
				| 'I' -> F.Imp(F.Var(c1),F.Var(c3))
				| 'E' -> F.Eq(F.Var(c1),F.Var(c3))
				| _ -> failwith "Operation attendue"
			end
			| c1::c2::[] -> if (c1 = 'N') then F.Not(F.Var(c2)) else failwith "Mauvaise negation"
			| c1::[] -> begin
				match c1 with 
				| 'F' -> F.Cst(false)
				| 'T' -> F.Cst(true)
				| _ -> F.Var(c1)
			end
			| _ -> failwith "Mauvaise longueur du Single"
		end
	| StringSplit (s1, c, s2) -> 
		begin
			match c with
			| 'A' -> F.And(builder s1, builder s2)
			| 'O' -> F.Or(builder s1, builder s2)
			| 'I' -> F.Imp(builder s1, builder s2)
			| 'E' -> F.Eq(builder s1, builder s2)
			| _ -> failwith "Operation attendue"
		end

	(* Normalise la String*)
	let authorised_caracters = ["=>" ; "<=>" ; "~" ; "and" ; "or" ; "(" ; ")" ; "true" ; "false" ; "#" ]
	let lexer s = Genlex.make_lexer authorised_caracters (Stream.of_string s)
	let normaliser s = 
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

end