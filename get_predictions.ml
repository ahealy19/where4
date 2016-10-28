open Printf
open Treetypes

(* traversing a tree recursively until a Leaf is encountered *)
let rec get_predictions_tree stats (index:int) (tree:decision_tree) : tree_node option  = 
	let n = tree.(index) in
	match n with
	| Node ((feature, threshold), is_true, is_false) ->
		(try
			let value = Hashtbl.find stats feature in
			if value <= threshold then
				get_predictions_tree stats is_true tree
			else get_predictions_tree stats is_false tree
		with Not_found -> eprintf "%s not found\n" feature; None) 
	| Leaf _ -> 
		(*printf "returning predictions!\n";*) (* for testing only!!*)
		Some n

(* not actually used - where4.ml has its own function which does this *)	
let print_predictions (n:tree_node option) : unit =
	match n with
	| Some tn -> (match tn with Leaf preds -> List.iter (fun (s,p) -> 
		printf "predicted value for %s: %f\n" s p) preds
		| Node _ -> eprintf "Unexpected non-leaf node as prediction. Exiting\n"; exit 1 )
	| None -> eprintf "No prediction supplied. Exiting\n"; exit 1 

(* now get a prediction for each prover by dividing by the number of trees *)
let average_from_forest (num_trees:float) (summed:tree_node) : tree_node option =
	match summed with
	| Leaf preds -> 
		Some (Leaf(List.map (fun (s, p) -> (s, p /. num_trees) )  preds))
	| Node _ -> None (* definitely shouldn't be node *)

(* each tree's predictions have to be combined by random forest in order to get their average *)
let combine_leaves (p:tree_node) (q:tree_node) : tree_node =
	match p with
	| Leaf preds1 ->
		(match q with
		| Leaf preds2 ->(
			(try
				let combined = List.combine preds1 preds2 in
				Leaf (List.map (fun ((s1, p1), (s2, p2)) ->
								if s1 <> s2 
								then (eprintf "provers have different names\n"; exit 1)
								else (s1, p1 +. p2) ) combined)
			with Invalid_argument _ ->
				eprintf "Leaf nodes of trees predict different numbers of provers\n"; exit 1)
		)
		| Node _ -> eprintf "Unexpected non-leaf node encountered. Exiting\n"; exit 1)
	| Node _ -> eprintf "Unexpected non-leaf node encountered. Exiting\n"; exit 1 

(* a forest's prediction is the average of its trees' prediction *)
let get_predictions_forest stats (forst:forest) : tree_node option = 
	(* Filtering out the Nones (no prediction there) *)
	let somes = List.filter 
		(fun a -> match a with Some _ -> true | None -> false) 
		(List.map (get_predictions_tree stats 0) forst) in
	let predictions = List.map 
		(fun a -> match a with Some p -> p | None -> eprintf "None slipped through"; exit 1) 
		somes in 
	(* now get the average of all these predictions *)
	match predictions with
	| hd :: tl ->   
	   (let summed = List.fold_left combine_leaves hd tl in
	   	average_from_forest (float_of_int (List.length predictions)) summed
		)
	| [] -> None

(* initial function for tree or forest *)	
let get_predictions stats : tree_node option =
	match Tree.tree with
	| Tree t -> get_predictions_tree stats 0 t
	| Forest f -> get_predictions_forest stats f

(* sorting based on cost *)
let sort_predictions pred : prediction list =
	match pred with
	| None -> [] | Some p -> (match p with 
		| Node _ -> []
		| Leaf preds -> (
	 		List.sort (fun (_, s1) (_, s2) -> if s1 > s2 then 1 else if s2 > s1 then (-1) else 0 )
	 		preds
	 	)
	)

(*safe way to extract provers from the head of a list using options*)
let get_best (preds:prediction list) = 
	match preds with [] -> None
	| (n,_)::tl -> Some(n) 

(* build with ocamlfind ocamlc -g -thread get_predictions.ml -o get_predictions.native *)