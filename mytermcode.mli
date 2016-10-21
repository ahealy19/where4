(* 	based on 'shape' computation function:
	val t_shape_task: ?version:int -> Task.task -> shape
 	from Why3: src/session/termcode.mli *)

open Why3

(* 	the function that traverses the AST of a Why3 Task.
	A hash table containing the number/value of each feature
	is returned. 

	This is encoded as feature (string) -> value (float)
 *)
val t_shape_num_map: Task.task -> (string, float) Hashtbl.t