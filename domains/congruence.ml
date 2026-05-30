open Frontend
open Frontend.AbstractSyntax

type t = Bot | Modulo of Z.t * Z.t (* x is Modulo(a, b) if x = a (mod b) *)

let pp fmt x =
    match x with
    | Bot -> Format.fprintf fmt "⊥@."
    | Modulo(a, b) -> Format.fprintf fmt "%s (mod %s)@." (Z.to_string a) (Z.to_string b)

let top = Modulo(Z.one, Z.zero)
let bottom = Bot
let is_bottom = (=) Bot

let const c = Modulo(c, Z.zero)

let rand a b = 
    (* je crois ? *)
    if a = b then const a else top

let unary x op =
    match op with
    | AST_UNARY_PLUS -> x
    | AST_UNARY_MINUS -> match x with
    | Bot -> Bot
    | Modulo(a, b) -> Modulo(Z.neg a, b)

let binary x y op =
    match op with
    | AST_PLUS -> (
        match x, y with
        | Bot, _ | _, Bot -> Bot
        | Modulo(x1, x2), Modulo(y1, y2) -> Modulo(Z.add x1 y1, Z.gcd x2 y2)
    )
    | AST_MINUS -> (
        match x, y with
        | Bot, _ | _, Bot -> Bot
        | Modulo(x1, x2), Modulo(y1, y2) -> Modulo(Z.sub x1 y1, Z.gcd x2 y2)
    )
    | AST_MULTIPLY -> (
        match x, y with
        | Bot, _ | _, Bot -> Bot
        | Modulo(x1, x2), Modulo(y1, y2) -> failwith "wip"
    )
    | AST_DIVIDE -> (
        match x, y with
        | Bot, _ | _, Bot -> Bot
        | Modulo(x1, x2), Modulo(y1, y2) -> failwith "wip"
    )
    | AST_MODULO -> (
        match x, y with
        | Bot, _ | _, Bot -> Bot
        | Modulo(x1, x2), Modulo(y1, y2) -> failwith "wip"
    )

let join x y = failwith "wip"
let meet x y = failwith "wip"
let widen = join

let compare x y op = failwith "wip"
let leq _ _ = failwith "wip"

let bwd_unary _ _ _ = failwith "not implemented"
let bwd_binary _ _ _ = failwith "not implemented"
let narrow _ _ = failwith "not implemented"
