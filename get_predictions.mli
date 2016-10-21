open Treetypes

(* 	given the feature vector returned from mytermcode.ml, 
	return the corresponding leaf node *)
val get_predictions: (string, float) Hashtbl.t -> tree_node option

(* if no proving is to be done, predictions can be printed *)
val print_predictions: tree_node option -> unit

val sort_predictions : tree_node option -> prediction list 

(* best solver installed locally, that is *)
val get_best : prediction list -> string option 
