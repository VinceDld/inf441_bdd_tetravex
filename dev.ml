(* bdt approche bottom up *)

exception RootExc of string
exception LeafExc of string
	type node  = 
			| Node of node ref * char * node ref * node option ref
			| Leaf of bool
		and bdt = {
			root: node;
			bottom: node ref list;
			}

	let setFather child newFather = match child with
		| Node(left, v, right, father) -> father := Some(newFather);
		| _ -> ()  

	let getFather child = match child with
		| Node(left, v, right, father) -> begin
			match !father with
			| None -> raise (RootExc("root has no father"))
			| Some x -> x
			end
		| _ -> failwith "No node"

	let getLeft node = match node with
		| Node(left, v, right, father) -> !left 
		| _ -> raise (LeafExc("leaf has no children"))

	let getRight node = match node with
		| Node(left, v, right, father) -> !right 
		| _ -> raise (LeafExc("leaf has no children"))

	let setLeft child newLeft = match child with
		| Node(left, v, right, father) -> left := newLeft;
		| _ -> () 

	let setRight child newRight = match child with
		| Node(left, v, right, father) -> right := newRight;
		| _ -> ()  

	let getLeafBool leaf = match leaf with
		| Leaf(b) -> b
		| _ -> failwith "not leaf" 

	let isLeaf node = match node with 
		| Leaf(b) -> true
		| _ -> false

	let isOptimisable node = match node with
		| Node (left, v, right, father) -> if isLeaf !left && isLeaf !right then getLeafBool !right = getLeafBool !left else false
		| _ -> false

	(* Applique l'operation op sur les 2 feuilles *)
	let op_and n1 n2 = Leaf(getLeafBool n1 && getLeafBool n2)


	(* Rien a optimiser sur neg car on conserve l'invariant *)
	let rec negNode refNode = match !refNode with 
		| Node(left, v, right, father) when (isLeaf !left && isLeaf !right)-> 
			left := Leaf(not (getLeafBool !left));
			right := Leaf(not (getLeafBool !right));
		| _ -> failwith "Structure de l'arbre pas cohérente"

(* opNode met directement à jour le noeud pointé par refNode en faisant op avec value *)
	let opNode op value refNode = match !refNode with
		| Node(left, v, right, father) when (isLeaf !left && isLeaf !right)-> 
			left := op !left value;
			right := op !right value;
		| _ -> failwith "Structure de l'arbre pas cohérente"

	let cloneBDT bdt =
		let currLeaves = ref [] in 
			let rec cloneNodeTree node =
			match node with 
				| Leaf(b) -> Leaf(getLeafBool node)
				| Node(left, v, right, father) -> 
				let nodeClone = Node(ref (cloneNodeTree(!left)), v, ref (cloneNodeTree (!right)), father) in
					if (isLeaf(!left)) then currLeaves := (ref nodeClone)::(!currLeaves);
					nodeClone;
			in { root = cloneNodeTree bdt.root; bottom = !currLeaves; }
	
	let nodeRefToString nodeP = 
		let rec aux node s = match node with
		| Leaf(b) -> if (b) then s^" T " else s^" F "
		| Node(left, v, right, father) -> aux (!right) ((aux !left s)^(String.make 1 v))
	in aux !nodeP ""

	let nodeToString nodeP = 
		let rec aux node s = match node with
		| Leaf(b) -> if (b) then s^" T " else s^" F "
		| Node(left, v, right, father) -> aux (!right) ((aux !left s)^(String.make 1 v))
	in aux nodeP ""

	let rec build_bdt fp = match fp with 
		| Var v -> let node =  Node(ref (Leaf(false)), v, ref (Leaf(true)), ref None) in 
					{ 
						root = node;
						bottom = [ref node];
					}
		| Cst b -> { root = Leaf(b); bottom = [] }
		| Not fp2 -> let bdt2 = build_bdt fp2 in 
					let tempBtd = {
						root = bdt2.root;
						bottom = bdt2.bottom;
					} in List.iter negNode tempBtd.bottom;
					tempBtd
		| And (fp1, fp2) -> let bdt1 = build_bdt fp1 in
			let bdt2 = build_bdt fp2 in
				let currBottom = ref [] in
				(* On rajoute btd2 a toutes les feuilles de bdt1 *)
				List.iter (function refNode1 ->
					let cloneBdt2 = cloneBDT bdt2 in
						match !refNode1 with 
						| Node(left, v, right, father) -> 
					(* On met a jour les valeurs des feuilles finales *)
						List.iter (opNode op_and !left) cloneBdt2.bottom;
						left := cloneBdt2.root;
						currBottom := cloneBdt2.bottom @ !currBottom;
						setFather cloneBdt2.root !refNode1;
						let cloneBdt21 = cloneBDT bdt2 in
							List.iter (opNode op_and !right) cloneBdt21.bottom;
							right := cloneBdt21.root;
							currBottom := cloneBdt21.bottom @ !currBottom;
							setFather cloneBdt21.root !refNode1;
						| _ -> failwith "Noeud attendu"
			) bdt1.bottom;
			{ root = bdt1.root; bottom = !currBottom; }
		| _ -> failwith "Erreur de build"


	let optimise bdt =
		let newBottom = ref [] in
			let rec aux liste =
		 	match liste with 
		 	| [] -> {
		 		root = bdt.root;
		 		bottom = !newBottom;
		 	}
		 	| x :: listeFin -> if isOptimisable !x then 
		 		begin 
		 		let pere = getFather !x in
		 		if getLeft pere = !x then
		 			setLeft pere (Leaf(getLeafBool (getLeft !x)))
		 		else setRight pere (Leaf(getLeafBool (getRight !x)));
		 		newBottom := (ref pere) :: !newBottom;
		 		aux listeFin
		 	end
		 	else 
		 	begin
		 		newBottom := x :: !newBottom;
		 		aux listeFin
		 	end
		 	in aux bdt.bottom


	let node = Node(ref (Leaf(false)), 'v', ref (Leaf(true)), ref None)
	let bdtInit = build_bdt (builder ("bAc"))
	let bdt = optimise bdtInit
	let _ = assert(nodeToString ( Node(ref (Leaf(false)), 'a', ref (Leaf(true)), ref None)) = " F a T ")

(*********************************************************************************
**********************************************************************************
**********************************************************************************
**********************************************************************************
**********************************************************************************)
(* bdd avec ref *)

type bddRef = 
	| NodeRef of bdtRef ref* char * bdtRef ref
	| LeafRef of bool
	| ToProcessRef

let bdt_to_bdtref nodeP = (* build the bdt with ref on the children *)
	let rec aux node = match node with
	| Leaf(b) -> if b then LeafRef(true)  else LeafRef(false)
	| Node(left, v, right) -> NodeRef(ref (aux left), v, ref (aux right))
	| ToProcess -> failwith "bdt mal construit"
	in aux nodeP

let bdtref_to_bdt nodeP = (* build the bdt with ref on the children *)
	let rec aux node = match node with
	| LeafRef(b) -> if b then Leaf(true)  else Leaf(false)
	| NodeRef(left, v, right) -> Node(aux !left, v, aux !right)
	| ToProcessRef -> failwith "bdt mal construit"
	in aux nodeP

(* On transcrit notre arbre en liste de références sur ses noeuds *)
let treeToListe tree = 
	let liste = ref [] in
		let rec aux node father =  (* Ajoute à liste les trucs *)
			match node with
			| ToProcessRef -> failwith ("Arbre mal construit")
			| LeafRef(b) -> () (* Rien à ajouter dans ce cas *)
			| NodeRef(left, v, right) -> 
				aux !right node;
				liste := ((ref node), father)::(!liste);
				aux !left node;
		in aux tree ToProcessRef;
	!liste

let changeChild node child newChild = match node with
	| NodeRef(left, v, right) -> if (!left = child) then left := newChild
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

let simplify tree =	
	let listeNodeRefFather = treeToListe tree in
		let rec aux liste = (*  parcourt la liste en faisant des join avec les nodes plus loin *)
			match liste with
			 | [] -> tree
			 | x :: l -> joinAlike l !(fst x) (snd x); (* On essaye de join x *)
			 			 aux l (* On continue de récurrer *)
		in aux listeNodeRefFather

(* Donne la hauteur en descendant à gauche *)
let rec leftheight bdtref = match bdtref with
	| LeafRef(b) -> 1
	| NodeRef(left,v,right) -> 1 + leftheight !left
	| _ -> failwith ("Erreur sur la hauteur")

(* Donne le nombre de sommet /!\ Détruit le bdtref *)
let rec number bdtref = match bdtref with
	| LeafRef(b) -> 0
	| NodeRef(left,v,right) -> let nb = number (!left) + 1 + number(!right) in 
		changeChild bdtref !left (LeafRef(true));
		changeChild bdtref !right (LeafRef(true));
		nb
	| _ -> failwith ("Erreur sur la hauteur")	

let a = make_bdt (Imp(Or(And(Var 'p', Var 'q'),And(Or(Var 'a', Var 'd'),Var 'y')),Var 'z'))
let b = bdtref_to_bdt (simplify (bdt_to_bdtref a))
let _ = print_int (number (simplify (bdt_to_bdtref a)))

(*********************************************************************************
**********************************************************************************
**********************************************************************************
**********************************************************************************
**********************************************************************************)
(* CODE PERMETTANT DE CREER UN BDT SIMPLE AVEC 0 OPTIMISATION*)
type node = 
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
	let _ = print_string ("Arbre prefixe : "^(bdt_to_string a));
