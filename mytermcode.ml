open Why3
open Why3.Term
open Why3.Task

(* to differenciate *)
let int_or_float c =
  match c with
  | Number.ConstInt _ -> "int"
  | Number.ConstReal _ -> "float"

(* after the tree has been traversed, categorise *)
let tidy tbl keys =
  let accum = (fun n k -> (Hashtbl.find tbl k) + n) in
  let count = List.fold_left accum 0 in
  let ops = ["and";"or";"impl";"let";"func";"not"] in
  let preds = ["case";"if";"iff"] in
  let leaves = ["var";"true";"false";"int";"float";"wild"] in 
  let n_leaves = count leaves in
  let n_preds = count preds in
  let n_quants = count ["forall"; "exists"] in
  let n_branches = (Hashtbl.find tbl "zero_ar") + n_leaves in
  let size = count 
      (List.filter (fun s -> s <> "depth" && s <> "avg_op_arity" && s <> "zero_ar") 
    keys) in
  Hashtbl.add tbl "divisor" (count ops);
  Hashtbl.add tbl "size" size;
  Hashtbl.add tbl "n_preds" n_preds;
  Hashtbl.add tbl "n_ops" (size - (n_quants+n_leaves));
  Hashtbl.add tbl "n_branches" n_branches;
  Hashtbl.add tbl "n_quants" n_quants;
  tbl

(* self explainatory *)
let rec get_arity t : int =
  match t.t_node with
  | Tconst _ | Tvar _ | Ttrue | Tfalse -> 1
  | Tnot _ | Tquant _ -> 1
  | Tbinop _ | Tlet _ | Teps _ -> 2
  | Tif _ -> 3
  | Tapp (_, l) -> List.length l
  | Tcase (t1, bl) -> List.length bl

(* we've got a leaf node *)
let update_depth tbl l =
  match l > (Hashtbl.find tbl "depth") with
  | true -> Hashtbl.replace tbl "depth" l; tbl
  | false -> tbl

(* when the arity n is known *)
let update_arity_n tbl n l = 
  let increment t k = (Hashtbl.replace t k ((Hashtbl.find t k)+1)) in
  (match n with
  | 0 -> let _ = update_depth tbl l in increment tbl "zero_ar"
  | _ -> ());
  let _ = Hashtbl.replace tbl "avg_op_arity" ((Hashtbl.find tbl "avg_op_arity")+n) in tbl

(* when the arity of t is not known *)
let update_arity tbl t l =
  update_arity_n tbl (get_arity t) l 

(* deal with patterns *)
let rec pat_shape_count_map ~(increment:'a -> string -> 'a) (level:int) (tbl:'a) p : 'a = 
  match p.pat_node with
  | Pwild -> increment (update_depth tbl level) "wild"
  | Pvar _ -> increment (update_depth tbl level) "var"
  | Papp (s,l) ->
    List.fold_left (pat_shape_count_map ~increment (level+1))
      (update_arity_n (increment tbl "func") (List.length l) level) l
  | Pas (p,_) ->
      increment (pat_shape_count_map ~increment (level+1) tbl p) "as"
  | Por (p,q) ->
      pat_shape_count_map ~increment (level+1) 
        (update_arity_n (increment (pat_shape_count_map ~increment (level+1) tbl q) "or") 2 level) 
        p

(* traverse the task AST, filling a hashtable 'tbl'  *)
let rec t_shape_count_map ~(increment:'a -> string -> 'a) (level:int) (tbl:'a) t : 'a = 
  let fn = t_shape_count_map ~increment (level+1) in
  match t.t_node with
  | Tconst c -> increment (update_depth tbl level) (int_or_float c)
  | Tvar _ -> increment (update_depth tbl level) "var"
  | Tapp (s,l) -> 
    List.fold_left fn (increment (update_arity tbl t level) "func") l
  | Tif (f,t1,t2) -> fn (fn (fn (increment (update_arity tbl t level) "if") t2) t1) f
  | Tcase (t1, bl) ->
    let br_shape acc b =
      let p, t2 = t_open_branch b in
      let tbl = t_shape_count_map ~increment (level+1) tbl t2 in
      pat_shape_count_map ~increment (level+1) tbl p
    in
    let tbl = (update_arity (increment tbl "case") t level) in
    let tbl = List.fold_left br_shape tbl bl in
    fn tbl t1
  | Teps b ->
    let _,f = t_open_bound b in
    t_shape_count_map ~increment (level+1) (increment tbl "eps") f
  | Tquant (q,b) ->
    let vl,triggers,f1 = t_open_quant b in
    let hq = match q with Tforall -> "forall" | Texists -> "exists" in
    let tbl = update_arity (increment (t_shape_count_map ~increment (level+1) tbl f1) hq) t level in
    List.fold_right
      (fun trigger acc ->
        List.fold_right
         (fun t acc ->
          t_shape_count_map ~increment (level+1) acc t)
         trigger acc)
      triggers tbl
  | Tbinop (o,f,g) ->
    let tag = match o with
    | Tand -> "and"
    | Tor -> "or"
    | Timplies -> "impl"
    | Tiff -> "iff"
    in
    fn (update_arity (increment (fn tbl g) tag) t level) f
  | Tlet (t1, b) ->
    let u,t2 = t_open_bound b in
    fn (update_arity (increment (t_shape_count_map ~increment (level+1) tbl t2) "let") t level) t1
  | Tnot f ->
    fn (update_arity (increment tbl "not") t level) f
  | Ttrue -> increment (update_depth tbl level) "true"
  | Tfalse -> increment (update_depth tbl level) "false" 

(* convert to float, convert avg to avg *)
let make_float_table itbl =
  let ftbl = Hashtbl.create 28 in
  Hashtbl.iter (fun k v -> Hashtbl.add ftbl k (float_of_int v)) itbl; 
  Hashtbl.replace ftbl "avg_op_arity" ((Hashtbl.find ftbl "avg_op_arity") /. (Hashtbl.find ftbl "divisor"));
  ftbl

(* initialise the hashtable, fill table, tidy table *)
let t_shape_num_map_term t =
  let level = 1 in
  let tbl = Hashtbl.create 28 in
  (* keys + 5  *)
  let keys = ["and";"or";"func";"case";"int";"float";"exists";"depth";"avg_op_arity";"as";"zero_ar"
            ;"wild";"not";"true";"false";"forall";"var";"iff";"if";"let";"impl";"eps"] in
  (* + size, n_branches, n_ops, n_quants *)
  (*  while traversing the term tree,
      - avg_op_arity maintains the TOTAL op arity (n_ops will to be divided by 
      this eventually to get the avg)
      - depth maintains the maximum depth of any branch encountered thus far 
    *)
  let _ = List.map (fun k -> Hashtbl.add tbl k 0) keys in
  let increment t k = (Hashtbl.replace t k ((Hashtbl.find t k)+1); t) in
  let pre_tidy = t_shape_count_map ~increment level tbl t in
  let post_tidy = tidy pre_tidy keys in 
  make_float_table post_tidy

(* visible from the interface *)
let t_shape_num_map t = (* t is a Task *)
  let f = Task.task_goal_fmla t in
  t_shape_num_map_term f
