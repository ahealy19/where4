open Format
open Why3
open Mytermcode
open Term
open Treetypes
open Get_predictions
open Printer
open Driver
open Make_session


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



let prove path_to_file (file:unit Session.file) hash : unit =
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
            (*let s =   match _pr.Call_provers.pr_answer with
              | Call_provers.Valid -> "Valid"
              | Call_provers.Invalid -> "Invalid"
              | Call_provers.Timeout -> "Timeout"
              | Call_provers.OutOfMemory -> "Fatal error: out of memory" 
              | Call_provers.StepLimitExceeded -> "Steps limit reached"
              | Call_provers.Unknown _ -> "Unknown"
              | Call_provers.Failure s -> "Failure " ^ s
              | Call_provers.HighFailure -> "Failure"
            in
            let pname = prover.Whyconf.prover_name in
            let steps = if _pr.Call_provers.pr_steps <> (-1) 
              then " ("^ (string_of_int ) _pr.Call_provers.pr_steps ^ " steps)"  else ""
            in printf "%a (%s : %.2f secs)%s\n" 
              s(*Call_provers.print_prover_answer _pr.Call_provers.pr_answer*)
              pname
              _pr.Call_provers.pr_time
              steps*)
            print_answer _pr prover;
          with e ->
            printf "Failure %a (%s)\n"
              Exn_printer.exn_printer e
              prover.Whyconf.prover_name   
    ) th.Session.theory_goals
  ) file.Session.file_theories

let print_predict path_to_file (file:unit Session.file) hash : unit =
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
  printf "Where4 version 1.0 (August 2016, Maynooth University)\n\n";
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
    | "-rel" ->
      if len > 3 then (which_fun Sys.argv.(3) (make_file Sys.argv.(3)) pm; exit 0) 
      else (printf "filename expected"; print_usage ())
    | _ -> which_fun Sys.argv.(2) (make_file (make_relative Sys.argv.(2))) pm; exit 0
  else printf "filename expected"; print_usage ()

let () = 
  let len = Array.length Sys.argv in
  if len > 1 then
    match Sys.argv.(1) with
    | "-h" | "--help" -> print_usage ()
    | "-v" | "--version" -> printf "Where4 version 1.0\n"
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

  
