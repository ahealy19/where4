(*open Core.Std*)
open Yojson
open Treetypes
(*open Format*)
open Printf
open Pervasives

let add_node n : tree_node =
	let t = n |> Yojson.Basic.Util.member "type" |> Yojson.Basic.Util.to_string in 		
	if t = "node" then
		(* these are the attributes we're interested in *)
		let feature = n |> Yojson.Basic.Util.member "feature" |> Yojson.Basic.Util.to_string in
		let threshold = n |> Yojson.Basic.Util.member "threshold" |> Yojson.Basic.Util.to_float in
		let is_true = n |> Yojson.Basic.Util.member "true" |> Yojson.Basic.Util.to_int in
		let is_false = n |> Yojson.Basic.Util.member "false" |> Yojson.Basic.Util.to_int in
		Node((feature, threshold), is_true, is_false)
	else 
		(* just use prover attributes to construct Leaf nodes. filter the rest *)
		Leaf(List.map 
			(fun (s,j) -> ( s, (Yojson.Basic.Util.to_float j) ))
			(List.filter (fun (k, _) -> k <> "type" && k <> "index") (Yojson.Basic.Util.to_assoc n)))

let make_tree json : decision_tree =
	(* nodes : (string * Basic.json) list list *)
	let nodes = json |> Yojson.Safe.to_basic |> Yojson.Basic.Util.to_list in
	(* n : (string * Basic.json) list *)
	Array.of_list (List.map (fun n -> add_node n)  nodes)

(* same as above without the conversion from Safe - this happens in make_forest *)
let make_tree_from_forest json : decision_tree =
	let nodes = Yojson.Basic.Util.to_list json in
	Array.of_list (List.map (fun n -> add_node n)  nodes)

let make_forest json : forest =
	let trees = json |> Yojson.Safe.to_basic |> Yojson.Basic.Util.to_list in
	List.map ((fun t -> make_tree_from_forest t)) trees

(** returns a table of feature -> value mappings for testing the get_predictions function  *)	
(*
let make_stats js =
	let json = js |> Yojson.Safe.to_basic |> Yojson.Basic.Util.index 0 |> Yojson.Basic.Util.to_assoc in 
	let table = String.Table.create () in

	List.iter json 
		~f:(fun (s, j) ->
			if s = "stats" then
				let fields = ["n_preds"; "avg_op_arity"; "depth"; "n_quants"; "n_ops"; "n_branches"; "size"] in
				List.iter fields 
				~f:(fun field -> 
					let value = j |> Yojson.Basic.Util.member field |> Yojson.Basic.Util.to_number in
					let _ = String.Table.add table ~key:field ~data:value in () );
				let types = ["and"; "wild"; "case"; "false"; "exists"; "int"; "float"; 
							"var"; "forall"; "iff"; "let"; "func"; "not"; "true"; "or"; "if"] in
				let n_types = j |> Yojson.Basic.Util.member "n_types"  in
				List.iter types
					~f:(fun t ->
						let s = n_types |> Yojson.Basic.Util.member t |> Yojson.Basic.Util.to_number in
						let _ = String.Table.add table ~key:t ~data:s in () )
			else ()
		); 
	table
*)
let print_node_file outc (n:tree_node) : unit =
	match n with
	| Node ((f, t), l, r) ->
		fprintf outc "\t\tNode((\"%s\",%f),%d,%d);\n" f t l r
	| Leaf preds -> 
		fprintf outc "\t\tLeaf([";
		List.iter (fun (s,p) -> fprintf outc "(\"%s\",%f);" s p ) preds;
		fprintf outc "]);\n"

let print_tree outc (tree:decision_tree) =
	fprintf outc "[|";
	Array.iter (print_node_file outc) tree;
	fprintf outc "|]"

let print_forest outc (forst:forest) = 
	fprintf outc "Forest([";
	List.iter (fun t -> print_tree outc t; fprintf outc ";\n") forst;
	fprintf outc "])"

let create_file filename (t_or_f:tree_or_forest) = 
	let outc = open_out filename in
	fprintf outc "open Treetypes\n\nlet tree = ";
	match t_or_f with
	| Tree t -> fprintf outc "Tree("; print_tree outc t; fprintf outc ")";
	| Forest f -> print_forest outc f;
	fprintf outc "\n";
	close_out outc

let create_interface filename = 
	let outc = open_out filename in
	fprintf outc "open Treetypes\n\nval tree : tree_or_forest\n";
	close_out outc

let get_json_name = function
	| "-" -> Yojson.Safe.from_file "forest.json"
	| filename -> Yojson.Safe.from_file filename

let main filename tree (*()*) =
	let json  = get_json_name filename in
	let t = match tree with true -> Tree (make_tree json) | false -> Forest (make_forest json) in
	create_file "tree.ml" t;
	create_interface "tree.mli";
	printf "tree.ml printed\n"
	(* the rest just tests that what is made is correct *)
	(*let stats = make_stats (Yojson.Safe.from_file "stats.json") in
	print_predictions (get_predictions stats t)*)

let print_usage () =

	printf "USAGE:\n\n";
	printf "\t<no-args>: looks for the file \'forest.json\' in the current dir\n";
	printf "\t-h,-help,--help,--help: print this message\n";
	printf "\t-v,-version,--version,--v: print version number\n";
	printf "\t-tree: looks for the file \'tree.json\' in the current dir\n";
	printf "\t-tree <filename>: the json file <filename> is loaded as a tree\n";
	printf "\t<filename>: the json file <filename> is loaded as a forest\n";
	exit 0

let one_arg a =
	match a with
	| "-help" | "--help" | "-h" | "--h" -> print_usage ()
	| "-version" | "--version" | "-v" | "--v" -> printf "print goals version 1.0\n"; exit 0
	| "-tree" | "true" -> (if (Array.length Sys.argv) < 3 then main "tree.json" true else main Sys.argv.(3) true)
	| "-forest" | "false" -> main "forest.json" false
	| _ -> main a false

(* build with ocamlfind ocamlc -g -thread -linkpkg -package yojson 
	print_goals.ml -o print_goals.native *)

let () =
	let l = Array.length Sys.argv in
	if l == 1 then
	 main "forest.json" false
	else if l < 4 then
	 one_arg Sys.argv.(1)
	else print_usage (); exit 1
	
	
	
	