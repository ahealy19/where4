open Format
open Why3
open Mytermcode
open Term
open Treetypes
open Get_predictions
open Printer
open Driver
open Make_session

type result = Call_provers.prover_result * string * string

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

(* print the static metrics as well as predicted best solver *)
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

let update_task (current:where_task) (new_result:result) (pr:Call_provers.prover_result) : where_task =
  { theory_name = current.theory_name;
    goal_name = current.goal_name;
    current_best = new_result;
    current_time = (current.current_time +. pr.Call_provers.pr_time) 
  }

let check_update (current:where_task) (new_result:result) : where_task =
  match new_result with
  | (pr, _, _) ->
    match pr.Call_provers.pr_answer with
    | Call_provers.Valid | Call_provers.Invalid-> 
      update_task current new_result pr
    | Call_provers.Unknown _ -> 
      begin
      match current.current_best with
      | (pr2, _, _) ->
        match pr2.Call_provers.pr_answer with
        | Call_provers.Unknown _ ->
          begin
          if pr.Call_provers.pr_time < current.current_time then
            update_task current new_result pr
          else 
            current
          end
        | _ -> update_task current new_result pr
      end
    | Call_provers.Timeout ->
      begin
        match current.current_best with
        | (pr2, _, _) ->
        match pr2.Call_provers.pr_answer with
        | Call_provers.Unknown _ -> 
          update_task current new_result pr
        | Call_provers.Timeout ->
          begin
          if pr.Call_provers.pr_time < current.current_time then
            update_task current new_result pr
          else 
            current
          end
        | _ -> update_task current new_result pr
      end
    | _ ->
      begin
        match current.current_best with
        | (pr2, _, _) ->
        match pr2.Call_provers.pr_answer with
        | Call_provers.Unknown _ | Call_provers.Timeout -> 
          update_task current new_result pr
        | _ ->
          begin
          if pr.Call_provers.pr_time < current.current_time then
            update_task current new_result pr
          else 
            current
          end
      end
      


let try_again (pr : result) : bool =
  match pr with 
  |(p,_,_) ->
    match p.Call_provers.pr_answer with
    | Call_provers.Valid | Call_provers.Invalid ->
      false
    | _ -> true



let rec best_I_have hash preds : ((Why3.Whyconf.config_prover * Why3.Driver.driver) option * prediction list) = 
  match preds with
  | [] -> (None,[])
  | (h, _)::tl -> (
      try
        let t = Hashtbl.find hash h in
        match t with
        | Some (p, d) -> (t, tl)
        | None -> best_I_have hash tl
      with Not_found ->
        best_I_have hash tl
    )

(* convert the prover answer into a string the Why3 driver understands *)
let print_answer (wtsk:where_task) : unit =
  match wtsk.current_best with
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
    in printf "%s (Theory: %s Goal: %s Prover: %s v.%s : %.2f secs)%s\n" 
      s
      wtsk.theory_name
      wtsk.goal_name
      pname
      pver
      wtsk.current_time
      steps



let get_prover_result p d tsk prover time : result option =
    try
      let pre_pc = Driver.prove_task 
         ~command:p.Whyconf.command
         (* time memory steps *)
        ~limit:(Call_provers.mk_limit time 1000 10000) d tsk in
      let pc = pre_pc () in
      let post_pc : Call_provers.post_prover_call =
      Call_provers.wait_on_call pc in
      let _pr : Call_provers.prover_result = post_pc () in
      Some (_pr, prover.Whyconf.prover_name, prover.Whyconf.prover_version)
    with e ->
      eprintf "Failure %a (%s)\n"
        Exn_printer.exn_printer e
        prover.Whyconf.prover_name;
      None

let rec exhaust_provers hash preds tsk time current : unit =
  let best, rest = best_I_have hash preds in
  match best with
  | None -> print_answer current(*eprintf "ERROR, none returned by best_I_have\n"*)
  | Some (p, d) -> 
    let prover : Whyconf.prover = p.Whyconf.prover in
    let _pr : result option = get_prover_result p d tsk prover time in 
    match _pr with
    | None -> ()
    | Some res ->
    let curr = check_update current res in
    (*print_answer curr; debugging*)
    if (try_again res) then
      exhaust_provers hash rest tsk time curr
    else 
      print_answer curr
  
    
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
  let best, _ = best_I_have hash static_order in

  match best with
  | None -> eprintf "ERROR, none returned by best_I_have\n"; None
  | Some (p, d) -> 
    let prover : Whyconf.prover = p.Whyconf.prover in 
    get_prover_result p d tsk prover time
  

(*  *)
let prove path_to_file (file:unit Session.file) hash time : unit =
  List.iter (
    fun th -> List.iter (
      fun g -> 
        let tsk = Session.goal_task g in
        (* give it to a good prover with a short timeour *)
        let _pr = initial_prover hash tsk 1 in
        match _pr with
        | None -> ()
        | Some pr ->
        let theory_name = th.Session.theory_name in
        let goal_name = g.Session.goal_name in
        match pr with
        | (pres,_,_) ->
        begin
        
        let curr = {
          theory_name = theory_name.Ident.id_string;
          goal_name = goal_name.Ident.id_string;
          current_best = pr;
          current_time = pres.Call_provers.pr_time 
          } in
        (*print_answer curr; debugging*)
        if (try_again pr) then
          (* feature extraction and prediction *)
          let preds = tsk |> get_stats |> get_predictions |> sort_predictions in
          exhaust_provers hash preds tsk (time-1) curr
        else
          print_answer curr

        end
    ) th.Session.theory_goals
  ) file.Session.file_theories

let print_predict path_to_file (file:unit Session.file) hash time : unit =
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
  printf "\t-h, --help\t\tprint this information\n";
  printf "\t-v, --version\t\tprint the version number\n";
  printf "\t-l, --list-provers\tlist the SMT provers found by Whyfolio which can be used for predictions\n";
  printf "\tprove -rel(optional) FILENAME\trun the predicted best prover on FILENAME (which must be a relative path to a .mlw or .why file \n\t\t\twhen using the -rel flag)\n";
  exit 0

let one_tab_or_two s =
  if String.length s < 6 then "\t\t" else "\t"

let prove_or_predict which_fun len =
  if len > 2 then
    let pm = provermap in
    match Sys.argv.(2) with
    | "-why" ->
      (* handle calls for why3 differently:
         the path needs to made relative to the tmp folder where why3 prints,
         a timit limit is given by the user via the driver 
       *)
      if len > 4 then (which_fun Sys.argv.(3) (make_file (make_relative Sys.argv.(3))) pm (int_of_string Sys.argv.(4)) ; exit 0) 
      else (printf "filename and time limit expected"; print_usage ())
    | _ -> which_fun Sys.argv.(2) (make_file Sys.argv.(2)) pm 10; exit 0
  else printf "filename expected"; print_usage ()

let () = 
  let len = Array.length Sys.argv in
  if len > 1 then
    match Sys.argv.(1) with
    | "-h" | "--help" -> print_usage ()
    | "-v" | "--version" -> printf "Where4 version 1.1\n"
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
    | "prove" -> prove_or_predict prove len
    | "predict" -> prove_or_predict print_predict len
    (* it will fall through to this if you're not careful *)
    | _ -> print_usage ()

  else print_usage ()

  
