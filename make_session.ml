open Why3
open Driver
open Format

(* partly based on <why-distrib>/examples/use_api/create_session.ml *)

(* reads the config file *)
let config : Whyconf.config ref = ref (Whyconf.read_config None)
(* the [main] section of the config file *)
let main : Whyconf.main ref = ref (Whyconf.get_main !config)

(* builds the environment from the [loadpath] *)
let env : Env.env ref = ref (Env.create_env (Whyconf.loadpath !main))

let dummy_keygen ?parent () = ()

(* Why3 format to store those intalled *)
let installed_provers : Whyconf.config_prover Whyconf.Mprover.t ref =
 ref (Whyconf.get_provers !config)

let esession e c =
  let dummy_session : unit Session.session = Session.create_session "." in
  let ctxt = {
    Session.allow_obsolete_goals = true;
    Session.release_tasks = false;
    Session.use_shapes_for_pairing_sub_goals = false;
    Session.keygen = dummy_keygen;
  }
  in
  let env_session,_,_ = Session.update_session ~ctxt:ctxt dummy_session e c in
  env_session

(* reads the file and returns an error if there's a problem. 
    resets the refs with updated values from the user's configpath *)
let make_file path_to_file configpath format : unit Session.file =
  try
    config := Whyconf.read_config (Some configpath);
    main := Whyconf.get_main !config;
    env := Env.create_env (Whyconf.loadpath !main);
    installed_provers := Whyconf.get_provers !config;
    Session.add_file ~keygen:dummy_keygen (esession !env !config) ~format:format path_to_file
  with e ->
    eprintf "@[Error while reading file@ '%s':@ %a@.@]" path_to_file
      Exn_printer.exn_printer e;
    exit 1 

let myprover_names = ["Z3-4.4.1"; "Z3-4.3.2"; "Alt-Ergo-1.01"; "Alt-Ergo-0.95.2";
                      "CVC4"; "CVC3"; "Yices"; "veriT"]

let z3regex = Str.regexp ".*Z3.*"
let altergo_regex = Str.regexp ".*ALT-?ERGO.*"
let cvc4_regex = Str.regexp ".*CVC4.*"
let cvc3_regex = Str.regexp ".*CVC3.*"
let veriT_regex = Str.regexp ".*VERIT.*"
let yices_regex = Str.regexp ".*YICES.*"

(* loading the drivers *)
(* and matching to my provers *)
(* this variable is accessed by where4.ml *)
let provermap =
  let pm = Hashtbl.create 10 in
  Whyconf.Mprover.iter
    (fun _ p ->
      try
        let d = Driver.load_driver !env p.Whyconf.driver [] in
        let add k = Hashtbl.add pm k (Some((p,d))) in (* function called below - given a key k *)
        let prover = p.Whyconf.prover in
        let parseable = Whyconf.prover_parseable_format prover in
        let regex = Str.regexp "," in
        let slist = Str.split regex parseable in (* solvers split by commas *)
        match slist with [] | _ :: [] -> ()
        | provername :: v :: _ -> (
          let up = String.uppercase provername in
          let find r = Str.string_match r up 0 && (List.length slist) < 3 (*no noBV*) in 
          if find z3regex then
            (match v with 
            | "4.4.1" | "4.4.0" -> add "Z3-4.4.1"
            | "4.3.2" -> add "Z3-4.3.2"
            | _ -> ())
          else if find altergo_regex then
            (match v with
              | "1.20.prv" | "1.10.prv" | "1.01" | "1.00.prv" -> add "Alt-Ergo-1.01"
              | "0.99.1" | "0.95.2" -> add "Alt-Ergo-0.95.2"
              | _ -> ())
          else if find cvc4_regex then
            (match v with "1.4" -> add "CVC4" | _ -> () )
          else if find cvc3_regex then
            (match v with "2.4.1" | "2.4" -> add "CVC3" | _ -> () )
          else if find veriT_regex then
            (match v with "201410" | "201506" -> add "veriT" | _ -> () )
          else if find yices_regex then
            (match v with "1.0.38" | "1.0.40" -> add "Yices" | _ -> () )
          else ()
        )
      with e ->
        let p = p.Whyconf.prover in
        eprintf "Failed to load driver for %s %s: %a@."
          p.Whyconf.prover_name p.Whyconf.prover_version
          Exn_printer.exn_printer e;
        exit 1) (* END of the big fun *)
    !installed_provers;
    List.iter (fun p ->
      if Hashtbl.mem pm p then () else Hashtbl.add pm p None
    ) myprover_names;(* not found provers get a value of None *)
    pm

(* needed by why3 which moves temporary files to /tmp/ *)
(* eg if in /home/andrew and given /tmp/whyfile.why:
    returns ../../tmp/whyfile.why which Why3 converts to
    ./../../tmp/whyfile.why automatically *)
let make_relative path_to_file =
	let sess_dir = Unix.getcwd () in
	let regex = Str.regexp "/" in
	let pieces = Str.split regex sess_dir in
	let backwards = String.concat "/" (List.map (fun _ -> "..") pieces) in
	String.concat "" [backwards; path_to_file]

