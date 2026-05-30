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
    
    let guard domain bexpr = domain 

    let fold_both d1 d2 f =
        VarMap.fold (fun var value dom ->
            match VarMap.find_opt var d2 with
            | Some(other) -> VarMap.add var (f value other) dom
            | None -> VarMap.add var value dom
        ) d1 init

    let join d1 d2 = (* we join each value in the domain *)
        fold_both d1 d2 Vd.join

    let meet d1 d2 = (* TODO : check *)
        fold_both d1 d2 Vd.meet

    let widen d1 d2 = (* TODO : check *)
        fold_both d1 d2 Vd.widen

    let leq d1 d2 =
        VarMap.fold (fun var value is_leq ->
            match VarMap.find_opt var d2 with
            | Some other -> is_leq && Vd.leq value other
            | None -> false
        ) d1 true

    let pp fmt =
        VarMap.iter (
            fun var value ->
                Format.fprintf fmt "%s <- " var.var_name;
                Vd.pp fmt value;
                Format.fprintf fmt "\n"
        )

    let narrow _ _ = failwith "not implemented"
end
