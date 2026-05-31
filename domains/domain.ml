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

module type DOMAIN_MAKE = functor(_: VARS)(_: ValueDomain.VALUE_DOMAIN) -> DOMAIN

module MakeForwardOnly(Vars: VARS)(Vd: ValueDomain.VALUE_DOMAIN) : DOMAIN = struct
    type t = Vd.t VarMap.t

    let init = List.fold_left (fun dom var -> VarMap.add var (Vd.const Z.zero) dom) VarMap.empty Vars.support
    let bottom = List.fold_left (fun dom var -> VarMap.add var Vd.bottom dom) VarMap.empty Vars.support
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
    
    let rec prop_not b =
        match b with
        | CFG_compare(AST_EQUAL, e1, e2) -> CFG_compare(AST_NOT_EQUAL, e1, e2)
        | CFG_compare(AST_NOT_EQUAL, e1, e2) -> CFG_compare(AST_EQUAL, e1, e2)
        | CFG_compare(AST_LESS, e1, e2) (* not(e1 < e2) <-> e1 >= e2 *) ->
                CFG_compare(AST_GREATER_EQUAL, e1, e2)
        | CFG_compare(AST_LESS_EQUAL, e1, e2) (* not(e1 <= e2) <-> e1 > e2 *) ->
                CFG_compare(AST_GREATER, e1, e2)
        | CFG_compare(AST_GREATER, e1, e2) (* not(e1 > e2) <-> e1 <= e2 *) ->
                CFG_compare(AST_LESS_EQUAL, e1, e2)
        | CFG_compare(AST_GREATER_EQUAL, e1, e2) (* not(e1 => e2) <-> e1 < e2 *) ->
                CFG_compare(AST_LESS, e1, e2)
        | CFG_bool_rand -> CFG_bool_rand
        | CFG_bool_const b -> CFG_bool_const (not b)
        | CFG_bool_unary (AST_NOT, b) -> b
        | CFG_bool_binary(AST_OR, e1, e2) ->
                CFG_bool_binary(AST_AND, prop_not e1, prop_not e2)
        | CFG_bool_binary(AST_AND, e1, e2) ->
                CFG_bool_binary(AST_OR, prop_not e1, prop_not e2)
    let join = (* we join each value in the domain *)
        VarMap.fold (fun var value dom ->
            match VarMap.find_opt var dom with
            | Some(other) -> VarMap.add var (Vd.join value other) dom
            | None -> VarMap.add var value dom
        )

    let meet d1 d2 = (* TODO : check *)
        if is_bottom d1 then bottom
        else VarMap.fold (fun var value dom ->
            if is_bottom dom then bottom (* [meet x bottom] always gives [bottom] *)
            else
                match VarMap.find_opt var d2 with
                | Some other ->
                        let value_meet_other = Vd.meet value other in
                        if Vd.is_bottom value_meet_other then
                            bottom
                        else VarMap.add var value_meet_other dom
                | None -> bottom
        ) d1 d2 

    let widen d1 d2 =
        VarMap.mapi (fun var _ ->
            let value = VarMap.find_opt var d1 |> Option.value ~default:Vd.bottom
            in
            Vd.widen value (VarMap.find var d2)
        ) (join d1 d2)
    
    (* [bwd dom expr result] is the domain that over approximates the domain
           that evaluates [expr] to [result] using [Vd.bwd_unary] and 
           [Vd.bwd_binary]*)
    let rec bwd dom expr result = 
        match expr with
        | CFG_int_const(a) -> if Vd.leq (Vd.const a) result then dom else bottom
        | CFG_int_rand(a, b) ->
            if Vd.is_bottom (Vd.meet result (Vd.rand a b)) then bottom
            else dom
        | CFG_int_var(var) ->
            let value = VarMap.find_opt var dom 
                |> Option.value ~default:Vd.bottom
                |> Vd.meet result (* unify the value found 
                                    with the expected result *)
            in
            VarMap.add var value dom
        | CFG_int_unary(op, expr) ->
            bwd dom expr (Vd.bwd_unary (get_value dom expr) op result)
        | CFG_int_binary(op, e1, e2) ->
            let result_left, result_right = Vd.bwd_binary (get_value dom e1) (get_value dom e2) op result in
            bwd (bwd dom e1 result_left) e2 result_right
    
    let rec guard dom bexpr =
        match bexpr with
        | CFG_bool_unary(AST_NOT, e) -> guard dom (prop_not e)
        | CFG_bool_binary(AST_AND, e1, e2) -> meet (guard dom e1) (guard dom e2)
        | CFG_bool_binary(AST_OR, e1, e2) -> join (guard dom e1) (guard dom e2)
        | CFG_bool_const(b) -> if b then dom else bottom
        | CFG_bool_rand -> dom
        | CFG_compare(op, e1, e2) ->
                let left, right = Vd.compare (get_value dom e1) (get_value dom e2) op in
                (* The guard limits the domain [dom] to those that evaluate
                   [e2] to [right] when [e1] is evaluated to [left], which
                   corresponds to making the expressions (hence the domain)
                   follow the approximation [Vd.compare] gives us *)
                bwd (bwd dom e1 left) e2 right

    let leq d1 d2 =
        VarMap.fold (fun var value is_leq ->
            match VarMap.find_opt var d2 with
            | Some other -> is_leq && Vd.leq value other
            | None -> false
        ) d1 true

    let pp fmt dom =
        Format.fprintf fmt "{";
        VarMap.iter (
            fun var value ->
                Format.fprintf fmt "%s = " var.var_name;
                Vd.pp fmt value;
                Format.fprintf fmt ", "
        ) dom;
        Format.fprintf fmt "}"

    let narrow _ _ = failwith "not implemented"
end
