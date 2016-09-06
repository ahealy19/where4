(********************************************************************)
(*                                                                  *)
(*  The Why3 Verification Platform   /   The Why3 Development Team  *)
(*  Copyright 2010-2015   --   INRIA - CNRS - Paris-Sud University  *)
(*                                                                  *)
(*  This software is distributed under the terms of the GNU Lesser  *)
(*  General Public License version 2.1, with the special exception  *)
(*  on linking described in file LICENSE.                           *)
(*                                                                  *)
(********************************************************************)

open Why3
open Why3.Term
open Why3.Task
(*open Core.Std.String.Table*)

(*******************************)
(*          explanations       *)
(*******************************)

let expl_regexp = Str.regexp "expl:\\(.*\\)"

let collect_expls lab =
  Ident.Slab.fold
    (fun lab acc ->
       let lab = lab.Ident.lab_string in
       if Str.string_match expl_regexp lab 0
       then Str.matched_group 1 lab :: acc
       else acc)
    lab
    []

let concat_expls = function
  | [] -> None
  | [l] -> Some l
  | l :: ls -> Some (l ^ " (" ^ String.concat ". " ls ^ ")")

let rec get_expls_fmla acc f =
  if f.t_ty <> None then acc
  else if Ident.Slab.mem Split_goal.stop_split f.Term.t_label then acc
  else
    let res = collect_expls f.Term.t_label in
    if res = [] then match f.t_node with
      | Term.Ttrue | Term.Tfalse | Term.Tapp _ -> acc
      | Term.Tbinop (Term.Timplies, _, f) -> get_expls_fmla acc f
      | Term.Tlet _ | Term.Tcase _ | Term.Tquant (Term.Tforall, _) ->
        Term.t_fold get_expls_fmla acc f
      | _ -> raise Exit
    else if acc = [] then res
    else raise Exit

let get_expls_fmla f = try get_expls_fmla [] f with Exit -> []

let goal_expl_task ~root task =
  let gid = (Task.task_goal task).Decl.pr_name in
  let info =
    let res = get_expls_fmla (Task.task_goal_fmla task) in
    concat_expls
      (if res <> [] && not root
       then res
       else collect_expls gid.Ident.id_label)
  in
  gid, info, task

(* {2 ident dictionaries for shapes} *)

let dict_table = Hashtbl.create 17
let dict_count = ref 0
let reverse_ident_table = Hashtbl.create 17
let reverse_dict_count = ref 0

let reset_dict () =
  Hashtbl.clear dict_table;
  Hashtbl.clear reverse_ident_table;
  dict_count := 0;
  reverse_dict_count := 0

(* {3 direct table to read shapes from strings} *)


let get_name s b i =
  try while !i < String.length s do
    match String.get s !i with
      | ')' -> incr i; raise Exit
      | c -> incr i; Buffer.add_char b c
    done;
    invalid_arg "Termcode.get_name: missing closing parenthesis"
  with Exit ->
    let id = Buffer.contents b in
    Hashtbl.add dict_table !dict_count id;
(*
    Format.eprintf "%d -> %s@." !dict_count id;
*)
    incr dict_count;
    id

let get_num s n i =
  try while !i < String.length s do
    match String.get s !i with
      | ')' -> incr i; raise Exit
      | '0'..'9' as c ->
        incr i; n := !n * 10 + (Char.code c - Char.code '0')
      | _ ->
        invalid_arg "Termcode.get_num: decimal digit expected"
    done;
    invalid_arg "Termcode.get_num: missing closing parenthesis"
  with Exit ->
    try Hashtbl.find dict_table !n
    with Not_found ->
      invalid_arg
        ("Termcode.get_num: invalid ident number " ^ string_of_int !n)

let get_id s i =
  if !i >= String.length s then
    invalid_arg "Termcode.get_id: missing closing parenthesis";
  match String.get s !i with
    | '0'..'9' as c ->
      let n = ref (Char.code c - Char.code '0') in
      incr i;
      get_num s n i
    | ')' -> invalid_arg "Termcode.get_id: unexpected closing parenthesis"
    | c ->
      let b = Buffer.create 17 in
      Buffer.add_char b c;
      incr i;
      get_name s b i

(* {2 Shapes} *)

type shape = string

let string_of_shape s = s


let shape_of_string s =
  try
  let l = String.length s in
  let r = Buffer.create l in
  let i = ref 0 in
  while !i < l do
  match String.get s !i with
    | '(' -> incr i; Buffer.add_string r (get_id s i)
    | c -> Buffer.add_char r c; incr i
  done;
  Buffer.contents r
  with e ->
    Format.eprintf "Error while reading shape [%s]@." s;
    raise e

(* tests
let _ = reset_dict () ; shape_of_string "a(b)cde(0)"
let _ = reset_dict () ; shape_of_string "a(bc)d(e)f(1)g(0)h"
let _ = reset_dict () ; shape_of_string "(abc)(def)(1)(0)(1)"
let _ = reset_dict () ; shape_of_string "(abcde)(fghij)(1)(0)(1)"
*)

let equal_shape (x:string) y = x = y
(* unused
let print_shape fmt s = Format.pp_print_string fmt (string_of_shape s)
*)

let debug = Debug.register_info_flag "session_pairing"
  ~desc:"Print@ debugging@ messages@ about@ reconstruction@ of@ \
         session@ trees@ after@ modification@ of@ source@ files."

let current_shape_version = 4

type shape_version = SV1 | SV2 | SV3

(* similarity code of terms, or of "shapes"

example:

  shape(forall x:int, x * x >= 0) =
         Forall(Int,App(infix_gteq,App(infix_st,Tvar 0,Tvar 0),Const(0)))
       i.e: de bruijn indexes, first-order term

*)

let vs_rename_alpha c h vs = incr c; Mvs.add vs !c h

let vl_rename_alpha c h vl = List.fold_left (vs_rename_alpha c) h vl

let rec pat_rename_alpha c h p = match p.pat_node with
  | Pvar v -> vs_rename_alpha c h v
  | Pas (p, v) -> pat_rename_alpha c (vs_rename_alpha c h v) p
  | Por (p, _) -> pat_rename_alpha c h p
  | _ -> Term.pat_fold (pat_rename_alpha c) h p


let tag_and = "A"
let tag_app = "a"
let tag_case = "C"
let tag_const = "c"
let tag_exists = "E"
let tag_eps = "e"
let tag_forall = "F"
let tag_false = "f"
let tag_impl = "I"
let tag_if = "i"
let tag_let = "L"
let tag_not = "N"
let tag_or = "O"
let tag_iff = "q"
let tag_true = "t"
let tag_var = "V"
let tag_wild = "w"
let tag_as = "z"


let id_string_shape ~push id acc =
  push id acc
(*
  let l = String.length id in
  if l <= 4 then push id acc else
  (* sanity check *)
  if (match String.get id 0 with
      | '0'..'9' -> true
      | _ ->
        try
          let (_:int) = String.index id ')' in
          true
        with Not_found -> false)
    then push id acc
    else push ")" (push id (push "(" acc))
*)

let get_ident_shape id =
  "a" ^ (id.Ident.id_string)



let int_or_float c =
  match c with
  | Number.ConstInt _ -> "int"
  | Number.ConstReal _ -> "float"
  
let ident_shape ~push id acc =
  id_string_shape ~push id.Ident.id_string acc

let get_const_shape c =
  Format.fprintf Format.str_formatter "%a" Pretty.print_const c;
  tag_const ^ (Format.flush_str_formatter ())

let const_shape ~push acc c =
  Format.fprintf Format.str_formatter "%a" Pretty.print_const c;
  push (Format.flush_str_formatter ()) acc

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

let rec get_arity t : int =
  match t.t_node with
  | Tconst _ | Tvar _ | Ttrue | Tfalse -> 1
  | Tnot _ | Tquant _ -> 1
  | Tbinop _ | Tlet _ | Teps _ -> 2
  | Tif _ -> 3
  | Tapp (_, l) -> List.length l
  | Tcase (t1, bl) -> List.length bl

let update_depth tbl l =
  match l > (Hashtbl.find tbl "depth") with
  | true -> Hashtbl.replace tbl "depth" l; tbl
  | false -> tbl

let update_arity_n tbl n l = 
  let increment t k = (Hashtbl.replace t k ((Hashtbl.find t k)+1)) in
  (match n with
  | 0 -> let _ = update_depth tbl l in increment tbl "zero_ar"
  | _ -> ());
  let _ = Hashtbl.replace tbl "avg_op_arity" ((Hashtbl.find tbl "avg_op_arity")+n) in tbl

let update_arity tbl t l =
  update_arity_n tbl (get_arity t) l 

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

let rec pat_shape_count ~(push:'a list->'a->('a list * 'a) list->('a list * 'a) list) (ctx:'a list) (acc:('a list * 'a) list) p : ('a list * 'a) list =
  match p.pat_node with
    | Pwild -> push ctx tag_wild acc
    | Pvar _ -> push ctx tag_var acc
    | Papp (s, l) ->
        let tag = (get_ident_shape s.ls_name) ^ "-" ^ (string_of_int (List.length l)) in
        List.fold_left (pat_shape_count ~push (tag::ctx))
          (push ctx tag acc)
          l
    | Pas (p, _) -> 
        push ctx tag_as (pat_shape_count ~push (tag_as::ctx) acc p)
    | Por (p, q) ->
        pat_shape_count ~push (tag_or::ctx) (push ctx tag_or (pat_shape_count ~push (tag_or::ctx) acc q)) p

let rec pat_shape ~(push:string->'a->'a) c m (acc:'a) p : 'a =
  match p.pat_node with
    | Pwild -> push tag_wild acc
    | Pvar _ -> push tag_var acc
    | Papp (f, l) ->
        List.fold_left (pat_shape ~push c m)
          (ident_shape ~push f.ls_name (push tag_app acc))
          l
    | Pas (p, _) -> push tag_as (pat_shape ~push c m acc p)
    | Por (p, q) ->
        pat_shape ~push c m (push tag_or (pat_shape ~push c m acc q)) p

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

let rec t_shape_count ~(push:'a list->'a->('a list * 'a) list->('a list * 'a) list) (ctx:'a list) (acc:('a list * 'a) list) t : ('a list * 'a) list =
  let fn tag = t_shape_count ~push (tag::ctx) in
  (*let arity = string_of_int (get_arity t) in*)
  match t.t_node with
  | Tconst c -> push ctx (get_const_shape c) acc
  | Tvar _ -> push ctx tag_var acc
  | Tapp (s,l) ->
      let tag = (get_ident_shape s.ls_name) ^ "-" ^ (string_of_int (List.length l)) in
      List.fold_left (fn tag)
        (push ctx tag acc)
        l
  | Tif (f,t1,t2) -> fn tag_if (fn tag_if (fn tag_if (push ctx tag_if acc) t2) t1) f
  | Tcase (t1,bl) ->
    let tag = tag_case ^ "-" ^ (string_of_int (List.length bl)) in
    let br_shape acc b = 
      let p,t2 = t_open_branch b in
      let acc = t_shape_count ~push (tag::ctx) acc t2 in
      pat_shape_count ~push (tag::ctx) acc p
    in
    let acc = push ctx tag acc in
    let acc = List.fold_left br_shape acc bl in
    fn tag acc t1
  | Teps b ->
    let _,f = t_open_bound b in
    t_shape_count ~push (tag_eps::ctx) (push ctx tag_eps acc) f
  | Tquant (q,b) ->
      let vl,triggers,f1 = t_open_quant b in
      let hq = match q with Tforall -> tag_forall | Texists -> tag_exists in
      let acc = push ctx hq (t_shape_count ~push (hq::ctx) acc f1) in
      List.fold_right
          (fun trigger acc ->
             List.fold_right
               (fun t acc ->
                  t_shape_count ~push (hq::ctx) acc t)
               trigger acc)
          triggers acc
  | Tbinop (o,f,g) ->
      let tag = match o with
      | Tand -> tag_and
      | Tor -> tag_or
      | Timplies -> tag_impl
      | Tiff -> tag_iff
      in
      fn tag (push ctx tag (fn tag acc g)) f
  | Tlet (t1,b) ->
     let u,t2 = t_open_bound b in
     fn tag_let (push ctx tag_let (t_shape_count ~push (tag_let::ctx) acc t2)) t1
  | Tnot f ->
      fn tag_not (push ctx tag_not acc) f
  | Ttrue -> push ctx tag_true acc
  | Tfalse -> push ctx tag_false acc

let rec t_shape ~version ~(push:string->'a->'a) c m (acc:'a) t : 'a =
  let fn = t_shape ~version ~push c m in
  match t.t_node with
    | Tconst c -> const_shape ~push (push tag_const acc) c
    | Tvar v ->
        let x =
          try string_of_int (Mvs.find v m)
          with Not_found -> v.vs_name.Ident.id_string
        in
        push x (push tag_var acc)
    | Tapp (s,l) ->
        List.fold_left fn
          (ident_shape ~push s.ls_name (push tag_app acc))
          l
    | Tif (f,t1,t2) ->
      begin match version with
      | SV1 | SV2 -> fn (fn (fn (push tag_if acc) f) t1) t2
      | SV3 -> fn (fn (fn (push tag_if acc) t2) t1) f
      end
    | Tcase (t1,bl) ->
        let br_shape acc b =
          let p,t2 = t_open_branch b in
          match version with
          | SV1 | SV2 ->
            let acc = pat_shape ~push c m acc p in
            let m = pat_rename_alpha c m p in
            t_shape ~version ~push c m acc t2
          | SV3 ->
            let m1 = pat_rename_alpha c m p in
            let acc = t_shape ~version ~push c m1 acc t2 in
            pat_shape ~push c m acc p
        in
        begin match version with
        | SV1 | SV2 ->
          List.fold_left br_shape (fn (push tag_case acc) t1) bl
        | SV3 ->
          let acc = push tag_case acc in
          let acc = List.fold_left br_shape acc bl in
          fn acc t1
        end
    | Teps b ->
        let u,f = t_open_bound b in
        let m = vs_rename_alpha c m u in
        t_shape ~version ~push c m (push tag_eps acc) f
    | Tquant (q,b) ->
        let vl,triggers,f1 = t_open_quant b in
        let m = vl_rename_alpha c m vl in
        let hq = match q with Tforall -> tag_forall | Texists -> tag_exists in
        (* argument first, intentionally, to give more weight on A in
           forall x,A *)
        let acc = push hq (t_shape ~version ~push c m acc f1) in
        List.fold_right
          (fun trigger acc ->
             List.fold_right
               (fun t acc ->
                  t_shape ~version ~push c m acc t)
               trigger acc)
          triggers acc
    | Tbinop (o,f,g) ->
        let tag = match o with
          | Tand -> tag_and
          | Tor -> tag_or
          | Timplies -> tag_impl
          | Tiff -> tag_iff
        in
        fn (push tag (fn acc g)) f
          (* g first, intentionally, to give more weight on B in A->B *)
    | Tlet (t1,b) ->
        let u,t2 = t_open_bound b in
        let m = vs_rename_alpha c m u in
        begin
          match version with
            | SV1 ->
              t_shape ~version ~push c m (fn (push tag_let acc) t1) t2
            | SV2 | SV3 ->
              (* t2 first, intentionally *)
              fn (push tag_let (t_shape ~version ~push c m acc t2)) t1
        end
    | Tnot f ->
      begin match version with
      | SV1 | SV2 -> push tag_not (fn acc f)
      | SV3 -> fn (push tag_not acc) f
      end
    | Ttrue -> push tag_true acc
    | Tfalse -> push tag_false acc

(* dead code
let t_shape_buf ?(version=current_shape_version) t =
  let b = Buffer.create 17 in
  let push t () = Buffer.add_string b t in
  let () = t_shape ~version ~push (ref (-1)) Mvs.empty () t in
  Buffer.contents b
*)

let make_float_table itbl =
  let ftbl = Hashtbl.create 28 in
  Hashtbl.iter (fun k v -> Hashtbl.add ftbl k (float_of_int v)) itbl; 
  Hashtbl.replace ftbl "avg_op_arity" ((Hashtbl.find ftbl "avg_op_arity") /. (Hashtbl.find ftbl "divisor"));
  ftbl

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


let t_shape_num_term t =
  let ctx = [] in
  let levels = [] in
  let push c tag lv = (c,tag)::lv in
  t_shape_count ~push ctx levels t

let t_shape_num_pat p =
  let ctx = [] in
  let levels = [] in
  let push c tag lv = (c,tag)::lv in
  pat_shape_count ~push ctx levels p

let t_shape_num_map t = (* t is a Task *)
  let f = Task.task_goal_fmla t in
  t_shape_num_map_term f

let t_shape_num t =
  let f = Task.task_goal_fmla t in
  t_shape_num_term f

let t_shape_task ~version t =
  let b = Buffer.create 17 in
  let push t () = Buffer.add_string b t in
  begin match version with
  | SV1 | SV2 -> ()
  | SV3 ->
    let _, expl, _ = goal_expl_task ~root:false t in
    match expl with
      | None -> ()
      | Some expl -> id_string_shape ~push expl ()
  end;
  let f = Task.task_goal_fmla t in
  let () = t_shape ~version ~push (ref (-1)) Mvs.empty () f in
  Buffer.contents b

let t_shape_task ?(version=current_shape_version) t =
  let version = match version with
    | 1 -> SV1 | 2 -> SV2 | 3 | 4 -> SV3
    | _ -> assert false
  in
  t_shape_task ~version t
