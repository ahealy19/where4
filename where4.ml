open Format
open Why3
open Mytermcode
open Term
open Treetypes
open Printer
open Driver
(* Why3 above, mine below *)
open Get_predictions
open Make_session

(* some record types used locally *)

type result = Call_provers.prover_result * string * string

type threshold = float option

type where_options = {
  thresh: threshold;
  verbose: bool;
  time: int;
  rel: bool;
  config: string;
}

type where_task = {
  theory_name: string;
  goal_name: string;
  current_best: result;
  current_time: float
}

(* a map for each metric -> value. Takes a Task *)
let get_stats t = Mytermcode.t_shape_num_map t

(* just print the statics metrics - for debugging. Takes a Task *)
let print_level_tags_goal stats : unit =
    Hashtbl.iter (fun k v -> if k="avg_op_arity" then printf "%s:%f\n" k v else printf "%s:%.0f\n" k v ) stats(*(get_stats t)*)

(* print the static metrics as well as predicted best solver .
  This functionality is not currently available from the command-line options
*)
let print_stats theory goals : unit = 
  let t_name : Ident.ident = theory.Session.theory_name in
  List.iter
    (fun g ->
      let g_name : Ident.ident = g.Session.goal_name in
      let t : Task.task = Session.goal_task g in
      let stats = get_stats t in
      printf "\ntheory:%s\ngoal:%s\n"
        t_name.Ident.id_string
        g_name.Ident.id_string;
    print_level_tags_goal stats;(*t;*)
    stats |> get_predictions |> print_predictions 
    (*print_predictions (get_predictions (get_stats t))*) 
    )
  goals

(* write the fields given a better result *)
let update_task (current:where_task) (new_result:result) (pr:Call_provers.prover_result) : where_task =
  { theory_name = current.theory_name;
    goal_name = current.goal_name;
    current_best = new_result;
    current_time = (current.current_time +. pr.Call_provers.pr_time) 
  }

(* for the total ordering of solver responses *)
let ans_to_num pr : int =
  match pr with
  | Call_provers.Valid -> 0
  | Call_provers.Invalid -> 1
  | Call_provers.Unknown _ -> 2
  | Call_provers.Timeout -> 3
  | _ -> 4 

(* is the new result better than the current best?
    if so, update the current best *)
let check_update (current:where_task) (new_result:result) : where_task =

 match new_result with
 | (pr1,_,_) -> 
 begin
   let p1 = (ans_to_num pr1.Call_provers.pr_answer) in
   match current.current_best with
   | (pr2,_,_) -> 
   begin
     let p2 = (ans_to_num pr2.Call_provers.pr_answer) in

     (* new result is a smaller answer  *)

     if p1 < p2 || (p1 = p2 && pr1.Call_provers.pr_time < pr2.Call_provers.pr_time) then 
        update_task current new_result pr1
     else
        current
   end
 end

(* check if another prover needs to be called *)
let try_again (pr : result) : bool =
  match pr with 
  |(p,_,_) ->
    match p.Call_provers.pr_answer with
    | Call_provers.Valid | Call_provers.Invalid ->
      false
    | _ -> true


(* returns the best (prover * driver) option installed locally, and the rest in case the best fails *)
let rec best_I_have hash preds : ((Why3.Whyconf.config_prover * Why3.Driver.driver) option * prediction list) = 
  match preds with
  | [] -> (None,[]) (* list has been exhausted *)
  | (h, _)::tl -> (
      try
        let t = Hashtbl.find hash h in
        match t with
        (* the best is installed *)
        | Some _ -> (t, tl) (* return the best and the rest *)
        (* best not installed, find the next best *)
        | None -> best_I_have hash tl
      with Not_found ->
        (* best not installed, find the next best *)
        best_I_have hash tl
    )

(* convert the prover answer into a string the Why3 driver understands
    and print it *)
let print_answer (wtsk:where_task) (to_print:result) : unit =
  match to_print with
  |(pr, pname, pver) ->
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
    let steps = if pr.Call_provers.pr_steps <> (-1) 
      then " ("^ (string_of_int ) pr.Call_provers.pr_steps ^ " steps)"  else ""
    in printf "%s\t(%.2f secs) %s v.%s\t-T %s -G \"%s\"%s\n" 
      s
      pr.Call_provers.pr_time
      pname
      pver
      wtsk.theory_name
      wtsk.goal_name
      steps


(* actually call p (config prover), given its driver d, using commands from 
    prover, with a time limit
    returns Some result if successful, None otherwise *)
let get_prover_result p d tsk prover time : result option =
    try
      let pre_pc = Driver.prove_task 
         ~command:p.Whyconf.command
         (* time memory steps *)
        ~limit:(Call_provers.mk_limit time 1000 10000) d tsk in
      let pc = pre_pc () in
      let post_pc : Call_provers.post_prover_call =
      (* blocks *)
      Call_provers.wait_on_call pc in
      let _pr : Call_provers.prover_result = post_pc () in
      Some (_pr, prover.Whyconf.prover_name, prover.Whyconf.prover_version)
    with e ->
      eprintf "Failure %a (%s)\n"
        Exn_printer.exn_printer e
        prover.Whyconf.prover_name;
      None

(* recursively call solvers until there are non left or Valid/Invalid is returned *)
(* args:  hash - the solvers+drivers and whether they are installed
          preds - predicted rank in order
          tsk - the Why3 Task. needed when calling get_prover_result
          time - time out value
          current - the best result so far
          verbose - boolean controlling printing *)
let rec exhaust_provers hash preds tsk time current verbose : unit =
  let best, rest = best_I_have hash preds in
  match best with
  | None -> print_answer current current.current_best
    let prover : Whyconf.prover = p.Whyconf.prover in
    let _pr : result option = get_prover_result p d tsk prover time in 
    match _pr with
    | None -> ()
    | Some res ->
    let curr = check_update current res in
    let () = if verbose then (print_answer curr res) in
    if (try_again res) then
      exhaust_provers hash rest tsk time curr verbose
    else 
      (* already printed above in verbose mode *)
      if (not verbose) then (print_answer curr curr.current_best) 
  
    
let initial_prover hash tsk time : result option =
  (* order derived from number of goals proved successfully, Table 1 F-IDE-2016 paper *)
  let static_order = [("Alt-Ergo-1.01", 0.0); 
                      ("CVC4",          1.0); 
                      ("CVC3",          2.0); 
                      ("Z3-4.4.1",      3.0);
                      ("Alt-Ergo-0.95.2",4.0);
                      ("Z3-4.3.2",      5.0);
                      ("Yices",         6.0); 
                      ("veriT",         7.0)] in
  (* get highest-ranking installed locally *)                    
  let best, _ = best_I_have hash static_order in

  match best with
  | None -> eprintf "ERROR, none returned by best_I_have\n"; None
  | Some (p, d) -> 
    let prover : Whyconf.prover = p.Whyconf.prover in 
    get_prover_result p d tsk prover time
  

(* schedule an initial solver and the entire rank if that one is unsuccessful *)
let prove path_to_file (file:unit Session.file) hash (opt:where_options) : unit =

  (* for each theory in the file, for each goal in the theory *)
  List.iter (
    fun th -> List.iter (
      fun g -> 
        let tsk = Session.goal_task g in
        (* give it to a good prover with a short timeout limit of 1 second *)
        let _pr = initial_prover hash tsk 1 in
        match _pr with
        | None -> ()
        | Some pr ->
        (let theory_name = th.Session.theory_name in
        let goal_name = g.Session.goal_name in
        match pr with
        | (pres,_,_) ->
        begin
        (* the current best result *)
        let curr = {
          theory_name = theory_name.Ident.id_string;
          goal_name = goal_name.Ident.id_string;
          current_best = pr;
          current_time = pres.Call_provers.pr_time 
          } in

        let () = if opt.verbose then print_answer curr pr in
        (* did pre-solving work? *)
        if (try_again pr) then
          (* feature extraction and prediction *)
          let sorted = tsk |> get_stats |> get_predictions |> sort_predictions in
          match opt.thresh with
          | Some t ->
            (* only provers with costs below the threshold *)
            let preds = List.filter (fun (_,f) -> f <= t) sorted in
            exhaust_provers hash preds tsk (opt.time) curr opt.verbose
          | None -> exhaust_provers hash sorted tsk (opt.time) curr opt.verbose
        else
          (* pre-solving successful *)
          print_answer curr pr
        end)
    ) th.Session.theory_goals
  ) file.Session.file_theories

(* for each theory in the file, for each goal in the theory, 
      extract metrics; traverse trees, sort predictions; print ranks of provers *)

let print_predict path_to_file (file:unit Session.file) hash (opt:where_options) : unit =
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
  printf "Where4 version 1.1 (October 2016, Maynooth University)\n\n";
  printf "USAGE:\n\n";
  printf "-h, --help\t\tprint this information\n";
  printf "--version\t\tprint the version number\n";
  printf "-l, --list-provers\tlist the SMT provers found by Where4 which can be used for predictions\n";
  printf "\n[prove|predict] FILENAME <[-ts|--threshold] THRESH> <[-tm|--time] TIME> <-v|--verbose>\n";
  printf "  prove\t\trun the best predicted provers on FILENAME\n";
  printf "  predict\t\tprint the best predicted provers for FILENAME\n";
  printf "  FILENAME\t\ta relative path to a .mlw or .why file (mandatory)\n";
  printf "----(the following flags are optional. Their order is unimportant)----\n";
  printf "  -ts,--threshold THRESH\tset the cost threshold to the floating point THRESH (optional)\n"; 
  printf "  -tm,--time TIME\t\tset the time limit for each prover to be the integer TIME (optional)\n";
  printf "  -v,--verbose\t\tprint out each solver answer (optional)\n";
  printf "  -c,--config PATH\t\tset the path to '.why.conf' (~/ by default)\n";
  exit 0

(*let print_opts (opt:where_options): unit=
  printf "time:%d\n" opt.time;
  let () = if opt.rel then printf "rel:true\n" else printf "rel:false\n" in
  let () = if opt.verbose then printf "verbose:true\n" else printf "verbose:false\n" in
  match opt.thresh with Some f -> (printf "thresh:%.2f" f) | None -> printf "thresh:none\n"*)

(* what kind of file is it? where is it and where is it going? *)
let prove_or_predict which_fun len opt =
  if len > 2 then
    let pm = provermap in
    let fn = if opt.rel then 
      (make_file Sys.argv.(2) opt.config "whyml")
    else
      (* handle calls for why3 differently:
         the path needs to made relative to the tmp folder where why3 prints,
         a timit limit is given by the user via the driver 
       *)
      (make_file (make_relative Sys.argv.(2)) opt.config "why")
    in
      (*printf "Format of %s: %s\n" path f; exit 0*) (* debugging *)
      which_fun Sys.argv.(2) fn pm opt; exit 0
  else printf "filename expected"; print_usage ()

(* only used for debugging *)
let print_opts (opt:where_options) : unit =
  let t = 
    match opt.thresh with
    | Some th -> th
    | None -> 0.0
    in
  let v = if opt.verbose then "true" else "false" in
  let rel = if opt.rel then "true" else "false" in
  printf "thresh: %.2f\nverbose: %s\ntime: %d\nrel: %s\nconfig: %s\n"
  t v opt.time rel opt.config   

(* checking that arguements make sense and returning them to the entry-point function *)
let rec extract_args (args:string list) (acc:where_options) : where_options option =
  match args with 
  | [] -> Some acc
  | h::t ->
  begin
    match h with 
    | "-v" | "--verbose" -> extract_args t {thresh=acc.thresh;verbose=true;time=acc.time;rel=acc.rel;config=acc.config}
    | "--why" -> extract_args t {thresh=acc.thresh;verbose=acc.verbose;time=acc.time;rel=false;config=acc.config}
    | "-ts" | "--threshold" ->
    begin
      match t with
      | hd::tl -> 
        (try
          extract_args tl {thresh=Some (float_of_string hd);verbose=acc.verbose;time=acc.time;rel=acc.rel;config=acc.config}
        with e -> printf "ERROR: THRESH must be a float eg: 7.5\n"; None) 
      | _ -> printf "-ts or --threshold must be followed by a float eg. 7.5\n"; None
    end
    | "-tm" | "--time" ->
    begin
      match t with 
      | hd::tl -> 
        (try
          extract_args tl {thresh=acc.thresh;verbose=acc.verbose;time=(int_of_string hd);rel=acc.rel;config=acc.config}
        with e -> printf "ERROR: TIME must be an integer eg. 5\n"; None) 
      | _ -> printf "-tm or --time must be followed by an integer eg. 5.\n"; None
    end
    | "-c" | "--config" ->
    begin
      match t with
      | hd::tl -> extract_args tl {thresh=acc.thresh;verbose=acc.verbose;time=acc.time;rel=acc.rel;config=hd}
      | _ -> printf "-c or --config must be followed by a path to .why3.conf\n"; None
    end
    | _ -> extract_args t acc
  end

let one_tab_or_two s =
  if String.length s < 6 then "\t\t" else "\t"

(* entry point for Where4 *)
let () = 
  let len = Array.length Sys.argv in
  (* process options to create a where_options record.
     it starts with some defaults. *)
  let o = extract_args (Array.to_list Sys.argv) {thresh=None;verbose=false;time=5;rel=true;config=((Sys.getenv "HOME")^"/.why3.conf")} in 
  match o with 
  | None -> exit 0 (* error caught *)
  | Some opt ->
  (*print_opts opt*) (*debugging*) 
  if len > 1 then
    match Sys.argv.(1) with
    | "-h" | "--help" -> print_usage ()
    | "--version" -> printf "Where4 version 1.1\n"
    | "-l" | "--list-provers" -> (
      printf "Known provers:\n\n";
      let pm = provermap in 
      Hashtbl.iter
      (fun k p -> 
        match p with
        | Some _ -> printf "%s%s ... found\n" k (one_tab_or_two k)
        | None -> printf "%s%s ... NOT found\n" k (one_tab_or_two k)
      ) pm
    )
    (* similar process used to predict as to prove - at least initially.
       pass to a function to determine what to do *)
    | "prove" -> prove_or_predict prove len opt
    | "predict" -> prove_or_predict print_predict len opt
    (* it will fall through to this if you're not careful *)
    | _ -> print_usage ()

  else print_usage ()

  
