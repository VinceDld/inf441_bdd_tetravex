open Parser;;
open P;;
open F;;

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
	
(* 	let joinToNodeBDT node bdt op = match node with
		| Node(left, v, right, father) -> 
			left := !(cloneBDT bdt).root.left;
		| _ -> failwith "Erreur dans joinToNodeBDT : un noeud est attendu pour join avec un BDT" *)

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
	(* let bdt = { root = Node(ref (Leaf(true)), 'v', ref node, ref None); bottom = [ref (Node(ref (Leaf(true)), 'v', ref node, ref None)); ref node];} *)
	let bdtInit = build_bdt (builder ("bAc"))
	let bdt = optimise bdtInit
	(* let _ = setLeft (!(List.hd bdt.bottom)) (Leaf(true)) *)
	let _ = List.iter print_string (List.map nodeRefToString bdt.bottom)
	let _ = print_string ("Arbre prefixe : "^(nodeToString (bdt.root)))
	let _ = assert(nodeToString ( Node(ref (Leaf(false)), 'a', ref (Leaf(true)), ref None)) = " F a T ")
(* 	let _ = assert(nodeRefToString ((cloneBDT bdt).root) = " T v T v F ");
	let _ = assert(List.map (function nodeRef -> nodeRefToString nodeRef) ((cloneBDT bdt).bottom) = [" T v F "; " T v T v F "]); *)

(* 	let  default = { 
		e_cst = (fun i -> Cst i);
		e_var = (fun s -> Var s);
		e_and = (fun e0 e1 -> Mul (e0 , e1)); 
		e_or = (fun e0 e1 -> Mul (e0 , e1)); 
		e_imp = (fun e0 e1 -> Mul (e0 , e1)); 
		e_eq = (fun e0 e1 -> Mul (e0 , e1)); 
		e_neg = (fun e -> Neg e); 
	}

	let  transf  vis =
		let rec aux node =
		match bdt with
			| Cst b -> vis.e_cst b
			| Var v -> vis.e_var v
			| And (e0, e1) -> vis.e_and (aux e0) (aux e1)
			| Or (e0, e1) -> vis.e_or (aux e0) (aux e1)
			| Imp (e0, e1) -> vis.e_imp (aux e0) (aux e1)
			| Eq (e0, e1) -> vis.e_eq (aux e0) (aux e1)
			| Neg e -> vis.e_neg (aux e)
		aux

	let  vis_cst_op op  =
		let  f_cst b =
			Cst (var_to_val  1) in
		{ default  with
		e_var = f_var }
		let  vis_var_def  var_to_val =
		transf (vis_var_def  var_to_val)


	let rec build_bdt0 f b = match b with
		|Empty ->  match f with
			|Var v -> Root(Node(False, v, True),(false::true::[]) ref)
			|Cst b -> if b then Root(True, [true] ref) else Root(False, [false] ref)
			|Not f -> Root(Node(True, v, False),(true::false::[]) ref)
			|And (f1,f2) |Or (f1,f2) |Imp (f1,f2) |Eq (f1,f2) -> build_bdt f2 (build_bdt f1 b) 
		|Root (n, l) -> match f with
			|Var v -> Root(Node(n, v, n),l)
			|Cst b -> Cst b
			|Not f -> Not (replace f (a,b))
			|And (f1,f2) ->  
			|Or (f1,f2) -> Or (replace f1 (a,b), replace f2 (a,b))
			|Imp (f1,f2) -> Imp (replace f1 (a,b), replace f2 (a,b))
			|Eq (f1,f2) -> Eq (replace f1 (a,b), replace f2 (a,b)) *)