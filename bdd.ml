open Parser;;
open P;;
open F;;

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

(* 	(* liste de tous les Node de l'arbre dans l'ordre préfixe *)
	let elements_prefixe t =
	let rec elements_append t l = match t with
		|Node (left,a,right) -> elements_append right (elements_append left l)@l
		|Leaf (a) -> Leaf(a)::l
		|ToProcess -> failwith "bdt mal construit"
	in elements_append t []

	let hash_of_bdt b =
		let my_hash = Hashtbl.create 1000 in
		let rec aux l i =match l with
			|[] -> my_hash
			|hd::tl -> Hashtbl.add my_hash i hd; aux tl (i+1) 
		in (aux (elements_prefixe b) 0) *)

 	let a = make_bdt (Or(Imp(Var 'p', Var 'q'),And(Var 'r', Var 's')))
	let b = make_bdt (Or(Var 'a', Var 'b'))
	let c = make_bdt (Var 'q')
(* 	let _ = assert *)
	let _ = print_string ((bdt_to_string a)); 

(* Partie sur la dernière compression *)
(* On transcrit notre arbre en liste de références sur ses noeuds *)
let treeToListe tree = 
	let liste = ref [] in
		let aux node father =  (* Ajoute à liste les trucs *)
			match node with
			| ToProcess -> failwith ("Arbre mal construit")
			| Leaf(b) -> () (* Rien à ajouter dans ce cas *)
			| Node(left, v, right) -> 
				aux !right node;
				liste := ((ref node), father)::(!liste);
				aux !left node;
		in aux tree ToProcess in
	!liste;

let changeChild node child newChild = match node with
	| Node(left, v, right) -> if (!left = child) then left := newChild
								else right := newChild
	| _ -> failwith ("Pas un noeud")


let joinAlike listeNodeRefFather node father = 
	(* find a node = to our node but with a different father and returns it *)
	let rec aux liste = 
		match liste with 
			| [] -> ()
			| x::l -> if !(fst x) = node && snd x != father then (* inutile de verif les fathers car (ref node, father) n'est pas dans la liste si elle est bien appelé *)
				changeChild father node !(fst x) (* On fait pointer le pere vers le nouveau fils *)
				(* On doit retirer node et ses fils ? Par forcément si on relie qu'avec des plus grands que nous dans la liste => pas de cycle qui se perd *)
						else aux l
	in aux listeNodeRefFather

let simplify tree =	let listeNodeRefFather = treeToListe tree in
		let rec aux liste = (*  parcourt la liste en faisant des join avec les nodes plus loin *)
			match liste with
			 | [] -> tree
			 | x :: l -> joinAlike l !(fst x) (snd x); (* On essaye de join x *)
			 			 aux l (* On continue de récurrer *)
		in aux listeNodeRefFather
	in tree


let a = make_bdt (Or(Imp(Var 'p', Var 'q'),And(Var 'r', Var 's')))
let b = simplify a
let _ = print_string ((bdt_to_string b)); 


(* Partie BDD *)
type node = 
	| Node of node*char*node*int 
	| Leaf of bool

type bdd = node*((int,node) Hashtbl.t)

let first_3 (a,b,c) = a
let second_3 (a,b,c) = b
let third_3 (a,b,c) = c

let file_to_hash filename = let file_to_hash_aux filename h=
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

let print_a_valuation l = List.iter (fun (a,b) -> print_char a; if b then print_string " @t\n" else print_string " @f\n") l


(* let b = fst (file_to_bdd "test.txt")


let _ = print_string (bdd_to_string b)
let _ = assert(b = Node(Node(Leaf true, 'r', Leaf true, 1), 'q', Leaf true, 0)) *) 

let l = get_a_true_valuation(fst (file_to_bdd "test.txt"))

let _ = print_a_valuation l

(* let my_hash = file_to_hash "test.txt" 
let a = Hashtbl.find my_hash "0" 
let c = Hashtbl.find my_hash "@t"
let _ = assert(c = ("","","")) 
let _ = assert(a = ("q","@t","1"))  *)

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