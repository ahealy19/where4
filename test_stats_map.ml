open Format
open Why3
open Mytermcode
open Term
open Treetypes
open Get_predictions
open Printer
open Driver


(* reads the config file *)
let config : Whyconf.config = Whyconf.read_config None
(* the [main] section of the config file *)
let main : Whyconf.main = Whyconf.get_main config

(* builds the environment from the [loadpath] *)
let env : Env.env = Env.create_env (Whyconf.loadpath main)

let dummy_keygen ?parent () = ()

let installed_provers : Whyconf.config_prover Whyconf.Mprover.t =
  Whyconf.get_provers config

let myprover_names = ["Z3-4.4.1"; "Z3-4.3.2"; "Alt-Ergo-1.01"; "Alt-Ergo-0.95.2";
                      "CVC4"; "CVC3"; "Yices"; "veriT"]

let z3regex = Str.regexp ".*Z3.*"
let altergo_regex = Str.regexp ".*ALT-?ERGO.*"
let cvc4_regex = Str.regexp ".*CVC4.*"
let cvc3_regex = Str.regexp ".*CVC3.*"
let veriT_regex = Str.regexp ".*VERIT.*"
let yices_regex = Str.regexp ".*YICES.*"

(* loading the drivers *)
let provermap =
  let pm = Hashtbl.create 10 in
  Whyconf.Mprover.iter
    (fun _ p ->
      try
        let d = Driver.load_driver env p.Whyconf.driver [] in
        let add k = Hashtbl.add pm k (Some((p,d))) in
        let prover = p.Whyconf.prover in
        let parseable = Whyconf.prover_parseable_format prover in
        let regex = Str.regexp "," in
        let slist = Str.split regex parseable in
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
    installed_provers;
    List.iter (fun p ->
      if Hashtbl.mem pm p then () else Hashtbl.add pm p None
    ) myprover_names;
    pm

(* a dummy keygen function for sessions *)
(* create an empty session in the current directory *)
let env_session,_,_ =
  let dummy_session : unit Session.session = Session.create_session "." in
  let ctxt = {
    Session.allow_obsolete_goals = true;
    Session.release_tasks = false;
    Session.use_shapes_for_pairing_sub_goals = false;
    Session.keygen = dummy_keygen;
  }
  in
  Session.update_session ~ctxt:ctxt dummy_session env config

(* adds a file in the new session *)

let make_file path_to_file : unit Session.file =
  try
    Session.add_file ~keygen:dummy_keygen env_session path_to_file
  with e ->
    eprintf "@[Error while reading file@ '%s':@ %a@.@]" path_to_file
      Exn_printer.exn_printer e;
    exit 1

(*let theories = file.Session.file_theories*)

let get_stats t = Mytermcode.t_shape_num_map t

let print_level_tags_goal t : unit =
    Hashtbl.iter (fun k v -> if k="avg_op_arity" then printf "%s:%f\n" k v else printf "%s:%.0f\n" k v ) (get_stats t)


let print_stats theory goals : unit = 
  let t_name : Ident.ident = theory.Session.theory_name in
  List.iter
    (fun g ->
      let g_name : Ident.ident = g.Session.goal_name in
      let t : Task.task = Session.goal_task g in
      printf "\ntheory:%s\ngoal:%s\n"
        t_name.Ident.id_string
        g_name.Ident.id_string;
    print_level_tags_goal t;
    print_predictions (get_predictions (get_stats t)) 
    )
  goals

(* temporarily necessary until I get a new forest *)
let old_to_new () =
  let tbl = Hashtbl.create 6 in
  Hashtbl.add tbl "Alt-Ergo" "Alt-Ergo-1.01";
  Hashtbl.add tbl "Z3" "Z3-4.4.1";
  Hashtbl.add tbl "CVC3" "CVC3";
  Hashtbl.add tbl "CVC4" "CVC4";
  Hashtbl.add tbl "Yices" "Yices";
  Hashtbl.add tbl "veriT" "veriT";
  tbl


let rec best_I_have hash preds : (Why3.Whyconf.config_prover * Why3.Driver.driver) option = 
  match preds with
  | [] -> None
  | (h, _)::tl -> (
      try
        let tbl = old_to_new () in
        let t = Hashtbl.find hash (Hashtbl.find tbl h) in
        match t with
        | Some (p, d) -> t
        | None -> best_I_have hash tl
      with Not_found ->
        eprintf "%s not found, exiting\n" h;
        exit 1 
    )


let print_answer (pr: Call_provers.prover_result) (p:Whyconf.prover) : unit =
  let s = match pr.Call_provers.pr_answer with
    | Call_provers.Valid -> "Valid"
    | Call_provers.Invalid -> "Invalid"
    | Call_provers.Timeout -> "Timeout"
    | Call_provers.OutOfMemory -> "Fatal error: out of memory" 
    | Call_provers.StepLimitExceeded -> "Steps limit reached"
    | Call_provers.Unknown _ -> "Unknown"
    | Call_provers.Failure s -> "Failure " ^ s
    | Call_provers.HighFailure -> "Failure"
  in
  let pname = p.Whyconf.prover_name in
  let steps = if pr.Call_provers.pr_steps <> (-1) 
    then " ("^ (string_of_int ) pr.Call_provers.pr_steps ^ " steps)"  else ""
  in printf "%s (%s : %.2f secs)%s\n" 
    s
    pname
    pr.Call_provers.pr_time
    steps


let prove (file:unit Session.file) hash : unit =
  List.iter (
    fun th -> List.iter (
      fun g -> 
        let tsk = Session.goal_task g in
        let best = tsk |> get_stats |> get_predictions |> sort_predictions |> best_I_have hash in 
        match best with None -> eprintf "ERROR, none returned by best_I_have\n" 
        | Some (p,d) ->
          let prover : Whyconf.prover = p.Whyconf.prover in 
          try
            let pre_pc = Driver.prove_task 
                ~command:p.Whyconf.command
                ~limit:(Call_provers.mk_limit 10 1000 10000) d tsk in
            let pc = pre_pc () in
            let post_pc : Call_provers.post_prover_call =
            Call_provers.wait_on_call pc in
            let _pr : Call_provers.prover_result = post_pc () in 
            print_answer _pr prover
          with e ->
            printf "Failure %a (%s)\n"
              Exn_printer.exn_printer e
              prover.Whyconf.prover_name   
    ) th.Session.theory_goals
  ) file.Session.file_theories

let print_predict path_to_file hash : unit =
  let file = make_file path_to_file in
  List.iter (
    fun th -> 
      let theory_name = th.Session.theory_name in
      List.iter (
      fun g -> 
        let tsk = Session.goal_task g in
        let sorted = tsk |> get_stats |> get_predictions |> sort_predictions in
        let goal_name = g.Session.goal_name in
        printf "%s : %s : %s\n" 
          path_to_file
          (theory_name.Ident.id_string)
          (goal_name.Ident.id_string);
        List.iteri (fun i (p,_) ->
          printf "\t%d : %s\n" (i+1) p
        ) sorted 
    ) th.Session.theory_goals
  ) file.Session.file_theories

let print_usage () = 
  printf "Whyfolio version 1.0 (August 2016, Maynooth University)\n\n";
  printf "USAGE:\n\n";
  printf "\t-h, --help\t\tprint this information\n";
  printf "\t-v, --version\t\tprint the version number\n";
  printf "\t-l, --list-provers\tlist the SMT provers found by Whyfolio which can be used for predictions\n";
  printf "\tprove <filename>\trun the predicted best prover on <filename> (which must be a relative path to a .mlw or .why file)\n";
  exit 0

let one_tab_or_two s =
  if String.length s < 6 then "\t\t" else "\t"

let () = 
  if Array.length Sys.argv > 1 then
    match Sys.argv.(1) with
    | "-h" | "--help" -> print_usage ()
    | "-v" | "--version" -> printf "Whyfolio version 1.0\n"
    | "-l" | "--list-provers" -> (
      let pm = provermap in 
      Hashtbl.iter
      (fun k p -> 
        match p with
        | Some _ -> printf "%s%s ... found\n" k (one_tab_or_two k)
        | None -> printf "%s%s ... NOT found\n" k (one_tab_or_two k)
      ) pm
    )
    | "prove" -> (
      if Array.length Sys.argv > 2 then
        let pm = provermap in
        prove (make_file Sys.argv.(2)) pm; exit 0
      else printf "filename expected"; print_usage ()
    )
    | "predict" -> (
      if Array.length Sys.argv > 2 then 
        let pm = provermap in
        print_predict Sys.argv.(2) pm; exit 0
      else printf "filename expected"; print_usage ()
    )
    (* it will fall through to this if you're not careful *)
    | _ -> print_usage ()

  else print_usage ()

  
