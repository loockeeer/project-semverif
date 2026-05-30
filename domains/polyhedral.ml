open Frontend
open Frontend.AbstractSyntax
open ControlFlowGraph

module Make(Vars : Domain.VARS)(Vd: ValueDomain.VALUE_DOMAIN) = struct
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

    let narrow _ _ = failwith "not implemented"
end
