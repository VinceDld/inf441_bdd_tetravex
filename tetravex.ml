(* let read_file  = 

	let chan = open_in "test.txt" in
	try
	  while true; do
	    let line = input_line chan in
	    print line;
	  done; 
	with End_of_file ->
	  close_in chan;; *)

(* let file_to_string filename =
	let rec aux c s =
		try 
			let line = input_line c in aux c (s^line^"\n")
		with
			End_of_file -> close_in c; s 
	in aux (open_in filename) "" *)

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
	in (aux "0",h)

let bdd_to_string nodeP = 
	let rec aux node = match node with
	| Leaf(b) -> if b then " T "  else " F "
	| Node(left, v, right, id) -> "{ " ^ (String.make 1 v) ^ " left son :" ^ (aux left) ^ " right son : " ^ (aux right) ^ " id :"^ (string_of_int id)^ "}"
in aux nodeP

let is_valid bdd = let h = snd(bdd) in not (Hashtbl.mem h (-2))

let is_satisfiable bdd = let h = snd(bdd) in (Hashtbl.mem h (-1))

let b = fst (file_to_bdd "test.txt")

let _ = print_string (bdd_to_string b)
let _ = assert(b = Node(Node(Leaf true, 'r', Leaf true, 1), 'q', Leaf true, 0)) 

let my_hash = file_to_hash "test.txt" 
let a = Hashtbl.find my_hash "0" 
let c = Hashtbl.find my_hash "@t"
let _ = assert(c = ("","","")) 
let _ = assert(a = ("q","@t","1")) 
(* let _ = assert(b = ("p","@f","@t"))  *)

(* let a = file_to_string "test.txt"
let _ = print_string a *)