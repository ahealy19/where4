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

(** Explanations *)

(* compiled with "ocamlfind ocamlc -c -linkpkg -package why3 mytermcode.mli" *)

open Why3

val goal_expl_task:
  root:bool -> Task.task -> Ident.ident * string option * Task.task

(** Shapes *)

val reset_dict : unit -> unit

val current_shape_version : int

type shape
val string_of_shape: shape -> string
val shape_of_string: string -> shape
val equal_shape: shape -> shape -> bool
(* unused
val print_shape: Format.formatter -> shape -> unit
*)

(* val t_shape_buf : ?version:int -> Term.term -> shape *)
  (** returns the shape of a given term *)
val t_shape_task: ?version:int -> Task.task -> shape
	(** return the shape of a given term *)
val t_shape_num_term: Term.term -> (string list * string) list
  (** returns the shape of a given task *)
val t_shape_num: Task.task -> (string list * string) list (* was (int * string) list *)

val t_shape_num_pat: Term.pattern -> (string list * string) list

val t_shape_num_map: Task.task -> (string, float) Hashtbl.t