(*
  Cours "Sémantique et Application à la Vérification de programmes"

  Ecole normale supérieure, Paris, France / CNRS / INRIA
*)

open Frontend
open! ControlFlowGraph

(* Signature for the variables *)

module type VARS = sig
  val support : var list
end

(*
  Signature of abstract domains representing sets of envrionments
  (for instance: a map from variable to their bounds).
 *)

module type DOMAIN = sig
  (* type of abstract elements *)
  (* an element of type t abstracts a set of mappings from variables
       to integers
     *)
  type t

  (* initial environment, with all variables initialized to 0 *)
  val init : t

  (* empty set of environments *)
  val bottom : t

  (* assign an integer expression to a variable *)
  val assign : t -> var -> int_expr -> t

  (* filter environments to keep only those satisfying the boolean expression *)
  val guard : t -> bool_expr -> t

  (* abstract join *)
  val join : t -> t -> t

  (* abstract meet *)
  val meet : t -> t -> t

  (* widening *)
  val widen : t -> t -> t

  (* narrowing *)
  val narrow : t -> t -> t

  (* whether an abstract element is included in another one *)
  val leq : t -> t -> bool

  (* whether the abstract element represents the empty set *)
  val is_bottom : t -> bool

  (* prints *)
  val pp : Format.formatter -> t -> unit
end

module type DOMAIN_MAKE = functor(_: ValueDomain.VALUE_DOMAIN) -> DOMAIN

module MakeForwardOnly(Vd: ValueDomain.VALUE_DOMAIN) : DOMAIN = struct
    type t = Vd.t VarMap.t

    let init = VarMap.empty
    let bottom = VarMap.empty
    let is_bottom = VarMap.exists (fun _ -> Vd.is_bottom)

    let rec get_value dom x =
        match x with
        | CFG_int_const i -> Vd.const i
        | CFG_int_rand (from_c, to_c) -> Vd.rand from_c to_c
        | CFG_int_unary (op, expr) -> Vd.unary (get_value dom expr) op
        | CFG_int_binary (op, e1, e2) -> Vd.binary  (get_value dom e1)
                                                    (get_value dom e2)
                                                    op
        | CFG_int_var(var) -> VarMap.find var dom

    let assign dom var iexpr =
        VarMap.add var (get_value dom iexpr) dom

    let guard dom bexpr = failwith "wip"
    let join d1 d2 = failwith "wip"
    let meet d1 d2 = failwith "wip"
    let widen d1 d2 = failwith "wip"

    let leq d1 d2 = failwith "wip"
    let pp fmt dom = failwith "wip"

    let narrow = failwith "not implemented"
end
