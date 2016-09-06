open Treetypes

val get_predictions: (string, float) Hashtbl.t (*-> tree_or_forest*) -> tree_node option

val print_predictions: tree_node option -> unit

val sort_predictions : tree_node option -> prediction list 

val get_best : prediction list -> string option 

(* complile with ocamlfind ocamlc -c  get_predictions.mli *)