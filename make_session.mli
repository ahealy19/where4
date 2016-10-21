open Why3

val make_file : string -> (unit Session.file) 

(* 	by default, Why3 adds './' to any path supplied.
	However, when called by the driver, an absolute
	path is supplied - this converts the absolute path
	to a relative one *)
val make_relative : string -> string

(* which solvers (known to Where4) are installed locally? *)
val provermap : (string, (Whyconf.config_prover * Driver.driver) option)
           Hashtbl.t