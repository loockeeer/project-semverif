open Frontend
open Frontend.AbstractSyntax

type t = Top | Bot | Integer of Z.t

let pp fmt x = Format.fprintf fmt "%s@." (
    match x with
    | Top -> "⊤"
    | Bot -> "⊥"
    | Integer z -> Z.to_string z
) 

let top = Top
let bottom = Bot
let is_bottom = (=) Bot

let const c = Integer c
let rand x y =
    (* if [x] = [y], obviously [rand x y] must be the integer they represent,
       otherwise, no integer value can represent them properly. *)
    if x = y then Integer x
    else if x > y then Bot
    else Top

let unary x op =
    match op with
    | AST_UNARY_PLUS -> x
    | AST_UNARY_MINUS -> match x with
        | Top | Bot -> x
        | Integer x -> Integer (Z.neg x)

let binary x y op =
    match x, y with
    | Bot, _ | _, Bot -> Bot
    | _ -> Top

let join x y = (* join keeps the best approximation *)
    match x, y with
    | z, Bot | Bot, z -> z
    | _ -> if x = y then x else Top

let meet x y = (* dual of join *)
    match x, y with
    | z, Top | Top, z -> z
    | _ -> if x = y then x else Bot

(* join ensures termination *)
let widen = join

let compare x y op = (x, y)
let leq x y = (* almost the same as for the sign domain *)
    match x, y with
    | Bot, _ | _, Top -> true
    | _ -> if x = y then true else false

let bwd_unary _ _ _ = failwith "not implemented"
let bwd_binary _ _ _ = failwith "not implemented"
let narrow _ _ = failwith "not implemented"
