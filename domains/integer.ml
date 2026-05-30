open Frontend
open Frontend.AbstractSyntax

type t = Top | Bot | Integer of Z.t

let pp fmt x = failwith "wip"

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
    | _ -> failwith "wip"

let join x y = failwith "wip"
let meet x y = failwith "wip"
let widen x y = failwith "wip"

let compare x y = failwith "wip"
let leq x y = failwith "wip"

let bwd_unary = failwith "not implemented"
let bwd_binary = failwith "not implemented"
let narrow = failwith "not implemented"
