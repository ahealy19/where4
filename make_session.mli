open Why3

val make_file : string -> (unit Session.file) 

val make_relative : string -> string

val provermap : (string, (Whyconf.config_prover * Driver.driver) option)
           Hashtbl.t