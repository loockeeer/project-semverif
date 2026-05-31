open Frontend
open Frontend.AbstractSyntax
open ControlFlowGraph
open Apron

exception Top

module Make (V : Domain.VARS) (_ : ValueDomain.VALUE_DOMAIN) : Domain.DOMAIN = struct
  open Apron

  (* manager *)
  type man = Polka.loose Polka.t
  let manager = Polka.manager_alloc_loose ()

  (* abstract elements *)
  type t = man Abstract1.t

  let vars =
    Array.of_list (List.map (fun v -> Var.of_string v.var_name) V.support)
  let env = Environment.make [||] vars

  type linear = { cst : int; coeffs : (string * int) list }

  let linear_zero = { cst = 0; coeffs = [] }

  let linear_of_const c = { cst = c; coeffs = [] }

  let linexpr_of_linear lin =
    let l = Linexpr1.make env in
    List.iter (fun (name, coeff) ->
      Linexpr1.set_coeff l (Var.of_string name) (Coeff.s_of_int coeff)
    ) lin.coeffs;
    Linexpr1.set_cst l (Coeff.s_of_int lin.cst);
    l

  let assign_constant abs var value =
    let le = Linexpr1.make env in
    Linexpr1.set_cst le (Coeff.s_of_int value);
    Abstract1.assign_linexpr manager abs (Var.of_string var.var_name) le None

  let add_coeff coeffs name delta =
    let rec aux acc = function
      | [] ->
          if delta = 0 then List.rev acc else List.rev ((name, delta) :: acc)
      | (current_name, current_coeff) :: rest when current_name = name ->
          let updated = current_coeff + delta in
          if updated = 0 then List.rev_append acc rest
          else List.rev_append acc ((name, updated) :: rest)
      | item :: rest -> aux (item :: acc) rest
    in
    aux [] coeffs

  let merge_linear sign left right =
    let coeffs =
      List.fold_left (fun acc (name, coeff) -> add_coeff acc name (sign * coeff))
        left.coeffs right.coeffs
    in
    { cst = left.cst + sign * right.cst; coeffs }

  let scale_linear factor lin =
    { cst = factor * lin.cst;
      coeffs = List.filter_map (fun (name, coeff) ->
          let coeff' = factor * coeff in
          if coeff' = 0 then None else Some (name, coeff')) lin.coeffs }

  let rec collect_linear e =
    match e with
    | CFG_int_const c ->
        linear_of_const (Z.to_int c)

    | CFG_int_var v ->
        { linear_zero with coeffs = [v.var_name, 1] }

    | CFG_int_unary (AST_UNARY_PLUS, e1) ->
        collect_linear e1

    | CFG_int_unary (AST_UNARY_MINUS, e1) ->
        scale_linear (-1) (collect_linear e1)

    | CFG_int_binary (AST_PLUS, e1, e2) ->
        merge_linear 1 (collect_linear e1) (collect_linear e2)

    | CFG_int_binary (AST_MINUS, e1, e2) ->
        merge_linear 1 (collect_linear e1) (scale_linear (-1) (collect_linear e2))

    | CFG_int_binary (AST_MULTIPLY, CFG_int_const c, e1)
    | CFG_int_binary (AST_MULTIPLY, e1, CFG_int_const c) ->
        scale_linear (Z.to_int c) (collect_linear e1)

    | _ -> raise Top

  let linexpr_of_int_expr e =
    linexpr_of_linear (collect_linear e)

  let make_lincons op e1 e2 =
    let diff = collect_linear (CFG_int_binary (AST_MINUS, e1, e2)) in
    let l = Linexpr1.make env in
    List.iter (fun (name, coeff) ->
      Linexpr1.set_coeff l (Var.of_string name) (Coeff.s_of_int coeff)
    ) diff.coeffs;
    Linexpr1.set_cst l (Coeff.s_of_int diff.cst);
    match op with
    | AST_EQUAL        -> Lincons1.make l Lincons1.EQ
    | AST_GREATER_EQUAL -> Lincons1.make l Lincons1.SUPEQ
    | AST_LESS_EQUAL   ->
        let neg = scale_linear (-1) diff in
        let l2 = Linexpr1.make env in
        List.iter (fun (name, coeff) ->
          Linexpr1.set_coeff l2 (Var.of_string name) (Coeff.s_of_int coeff)
        ) neg.coeffs;
        Linexpr1.set_cst l2 (Coeff.s_of_int neg.cst);
        Lincons1.make l2 Lincons1.SUPEQ
    | AST_GREATER      ->
      Linexpr1.set_cst l (Coeff.s_of_int (diff.cst - 1));
      Lincons1.make l Lincons1.SUPEQ
    | AST_LESS         ->
        let neg = scale_linear (-1) diff in
        let l2 = Linexpr1.make env in
        List.iter (fun (name, coeff) ->
          Linexpr1.set_coeff l2 (Var.of_string name) (Coeff.s_of_int coeff)
        ) neg.coeffs;
        Linexpr1.set_cst l2 (Coeff.s_of_int (neg.cst - 1));
        Lincons1.make l2 Lincons1.SUPEQ
    | AST_NOT_EQUAL    -> raise Top

  let apply_lincons abs c =
    let ar = Lincons1.array_make env 1 in
    Lincons1.array_set ar 0 c;
    Abstract1.meet_lincons_array manager abs ar

  let diff_linexpr e1 e2 =
    linexpr_of_int_expr (CFG_int_binary (AST_MINUS, e1, e2))

  let invert_compare = function
    | AST_EQUAL         -> AST_NOT_EQUAL
    | AST_NOT_EQUAL     -> AST_EQUAL
    | AST_LESS          -> AST_GREATER_EQUAL
    | AST_LESS_EQUAL    -> AST_GREATER
    | AST_GREATER       -> AST_LESS_EQUAL
    | AST_GREATER_EQUAL -> AST_LESS

  let rec prop_not b =
    match b with
    | CFG_compare (op, e1, e2) -> CFG_compare (invert_compare op, e1, e2)
    | CFG_bool_const b -> CFG_bool_const (not b)
    | CFG_bool_rand -> CFG_bool_rand
    | CFG_bool_unary (AST_NOT, b) -> b
    | CFG_bool_binary (AST_AND, b1, b2) ->
        CFG_bool_binary (AST_OR, prop_not b1, prop_not b2)
    | CFG_bool_binary (AST_OR, b1, b2) ->
        CFG_bool_binary (AST_AND, prop_not b1, prop_not b2)

  let init   = Abstract1.top   manager env

  let bottom = Abstract1.bottom manager env

  let int_of_scalar = function
    | Scalar.Float f -> int_of_float f
    | Scalar.Mpqf q -> int_of_float (Mpqf.to_float q)
    | Scalar.Mpfrf f -> int_of_float (Mpfrf.to_float ~round:Mpfr.Near f)

  let singleton_interval_int interval =
    if interval.Interval.inf = interval.Interval.sup then
      Some (int_of_scalar interval.Interval.inf)
    else
      None

  let const_of_expr abs = function
    | CFG_int_const c -> Some (Z.to_int c)
    | CFG_int_var v ->
        singleton_interval_int
          (Abstract1.bound_variable manager abs (Var.of_string v.var_name))
    | _ -> None

  let assign abs var expr =
    let v = Var.of_string var.var_name in
    match expr with

    | CFG_int_rand (l, u) ->
        let abs = Abstract1.forget_array manager abs [|v|] false in
        let c1 =
          let lcons = Lincons1.make (Linexpr1.make env) Lincons1.SUPEQ in
          Lincons1.set_coeff lcons (Var.of_string var.var_name) (Coeff.s_of_int 1);
          Lincons1.set_cst lcons (Coeff.s_of_int (- Z.to_int l));
          lcons
        in
        let c2 =
          let lcons = Lincons1.make (Linexpr1.make env) Lincons1.SUPEQ in
          Lincons1.set_coeff lcons (Var.of_string var.var_name) (Coeff.s_of_int (-1));
          Lincons1.set_cst lcons (Coeff.s_of_int (Z.to_int u));
          lcons
        in
        let ar = Lincons1.array_make env 2 in
        Lincons1.array_set ar 0 c1;
        Lincons1.array_set ar 1 c2;
        Abstract1.meet_lincons_array manager abs ar

    | _ ->
        (match expr with
        | CFG_int_binary (AST_DIVIDE, e1, e2) ->
            (match const_of_expr abs e1, const_of_expr abs e2 with
             | _, Some 0 -> bottom
             | Some a, Some b ->
                 assign_constant abs var (a / b)
             | _, Some 1 ->
                 (try
                    let le = linexpr_of_int_expr e1 in
                    Abstract1.assign_linexpr manager abs v le None
                  with _ -> raise Top)
             | _, Some (-1) ->
                 (try
                    let le = linexpr_of_int_expr (CFG_int_unary (AST_UNARY_MINUS, e1)) in
                    Abstract1.assign_linexpr manager abs v le None
                  with _ -> raise Top)
             | _ -> raise Top)
        | CFG_int_binary (AST_MODULO, e1, e2) ->
            (match const_of_expr abs e1, const_of_expr abs e2 with
             | _, Some 0 -> bottom
             | Some a, Some b ->
                 assign_constant abs var (a mod b)
             | _, Some 1
             | _, Some (-1) -> assign_constant abs var 0
             | _ -> raise Top)
         | CFG_int_binary (AST_MULTIPLY, e1, e2) ->
             let is_zero_expr e =
               match e with
               | CFG_int_const c -> Z.equal c Z.zero
               | CFG_int_var var ->
                   let bound = Abstract1.bound_variable manager abs (Var.of_string var.var_name) in
                   Interval.is_zero bound
               | _ -> false
             in
             let assign_scaled factor expr =
               let le = linexpr_of_linear (scale_linear factor (collect_linear expr)) in
               Abstract1.assign_linexpr manager abs v le None
             in
             let constant_factor expr = const_of_expr abs expr in
             if is_zero_expr e1 || is_zero_expr e2 then
               assign_constant abs var 0
             else
               (try
                  match constant_factor e1, constant_factor e2 with
                  | Some k, _ when k <> 0 -> assign_scaled k e2
                  | _, Some k when k <> 0 -> assign_scaled k e1
                  | _ -> raise Top
                with _ ->
                  (try
                     match e1, e2 with
                     | CFG_int_var v1, CFG_int_var v2 ->
                         let b1 = Abstract1.bound_variable manager abs (Var.of_string v1.var_name) in
                         let b2 = Abstract1.bound_variable manager abs (Var.of_string v2.var_name) in
                         let s1 = b1.Interval.inf and t1 = b1.Interval.sup in
                         let s2 = b2.Interval.inf and t2 = b2.Interval.sup in
                         let a = int_of_scalar s1 in
                         let b = int_of_scalar t1 in
                         let c = int_of_scalar s2 in
                         let d = int_of_scalar t2 in
                         let candidates = [a*c; a*d; b*c; b*d] in
                         let mn = List.fold_left min (List.hd candidates) candidates in
                         let mx = List.fold_left max (List.hd candidates) candidates in
                         let c1 = Lincons1.make (Linexpr1.make env) Lincons1.SUPEQ in
                         Lincons1.set_coeff c1 (Var.of_string var.var_name) (Coeff.s_of_int 1);
                         Lincons1.set_cst c1 (Coeff.s_of_int (-mn));
                         let l2 = Linexpr1.make env in
                         Linexpr1.set_coeff l2 (Var.of_string var.var_name) (Coeff.s_of_int (-1));
                         Linexpr1.set_cst l2 (Coeff.s_of_int mx);
                         let c2 = Lincons1.make l2 Lincons1.SUPEQ in
                         let ar = Lincons1.array_make env 2 in
                         Lincons1.array_set ar 0 c1;
                         Lincons1.array_set ar 1 c2;
                         Abstract1.meet_lincons_array manager abs ar
                     | _ -> raise Top
                   with _ ->
                     try
                       let le = linexpr_of_int_expr expr in
                       Abstract1.assign_linexpr manager abs v le None
                     with _ -> Abstract1.forget_array manager abs [|v|] false))
         | _ ->
             try
               let le = linexpr_of_int_expr expr in
               Abstract1.assign_linexpr manager abs v le None
             with _ -> Abstract1.forget_array manager abs [|v|] false)

  let guard_compare abs op e1 e2 =
    match op with
    | AST_NOT_EQUAL ->
        (match e1, e2 with
         | CFG_int_var v, CFG_int_const c
         | CFG_int_const c, CFG_int_var v ->
             let bound = Abstract1.bound_variable manager abs (Var.of_string v.var_name) in
             if Interval.equal_int bound (Z.to_int c) then bottom else
               let a1 = try apply_lincons abs (make_lincons AST_LESS e1 e2)
                        with _ -> abs in
               let a2 = try apply_lincons abs (make_lincons AST_GREATER e1 e2)
                        with _ -> abs in
               Abstract1.join manager a1 a2
         | _ ->
             (try
                let eq_cons = make_lincons AST_EQUAL e1 e2 in
                let abs_eq = apply_lincons abs eq_cons in
                if Abstract1.is_eq manager abs abs_eq then bottom
                else
                  let diff = diff_linexpr e1 e2 in
                  let bound = Abstract1.bound_linexpr manager abs diff in
                  if Interval.is_zero bound then bottom
                  else
                    let a1 = try apply_lincons abs (make_lincons AST_LESS e1 e2)
                             with _ -> abs in
                    let a2 = try apply_lincons abs (make_lincons AST_GREATER e1 e2)
                             with _ -> abs in
                    Abstract1.join manager a1 a2
              with _ ->
                let a1 = try apply_lincons abs (make_lincons AST_LESS e1 e2)
                         with _ -> abs in
                let a2 = try apply_lincons abs (make_lincons AST_GREATER e1 e2)
                         with _ -> abs in
                Abstract1.join manager a1 a2))
    | _ ->
        try apply_lincons abs (make_lincons op e1 e2)
        with _ -> abs

  let rec guard abs bexpr =
    try
      match bexpr with
      | CFG_bool_const true  -> abs
      | CFG_bool_const false -> bottom
      | CFG_bool_rand        -> abs

      | CFG_compare (op, e1, e2) ->
          (try guard_compare abs op e1 e2 with _ -> abs)

      | CFG_bool_unary (AST_NOT, b) ->
          guard abs (prop_not b)

      | CFG_bool_binary (AST_AND, b1, b2) ->
          guard (guard abs b1) b2

      | CFG_bool_binary (AST_OR, b1, b2) ->
          Abstract1.join manager (guard abs b1) (guard abs b2)

    with _ -> abs

  let join   = Abstract1.join manager
  let meet   = Abstract1.meet manager
  let widen  = Abstract1.widening manager
  let narrow = meet
  let leq x y = Abstract1.is_leq manager x y
  let is_bottom x = Abstract1.is_bottom manager x
  let pp fmt x = Abstract1.print fmt x
end