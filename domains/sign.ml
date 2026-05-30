open Frontend
open Frontend.AbstractSyntax

type t = Zero | Pos | Neg | Top | Bot

let pp fmt x = failwith "wip"

let top = Top
let bottom = Bot
let is_bottom = (=) Bot

let const c = if c > Z.zero then Pos else if c = Z.zero then Zero else Neg
let rand a b =
    (* [rand a b] is [Top] when the sign can't be guessed, i.e. when
       [a] and [b] have differing signs, or only one of them is zero, or
       only one is Top/Bot.
       Hence, *)
    if const a = const b then const b else top

let unary x op =
    match op with
    | AST_UNARY_PLUS -> x
    | AST_UNARY_MINUS -> match x with
        | Top | Bot | Zero -> x
        | Neg -> Pos
        | Pos -> Neg
let binary x y op =
    match op with
    | AST_PLUS -> (
        match x, y with
        | Pos, Pos -> Pos
        | Neg, Neg -> Neg
        | Zero, Zero -> Zero
        | (Pos | Neg as c), Zero | Zero, (Pos | Neg as c) -> c
        | Bot, _ | _, Bot -> Bot
        | Pos, Neg | Neg, Pos | Top, _ | _, Top-> Top
    )
    | AST_MINUS -> (
        match x, y with
        | (Zero | Neg), Pos -> Neg
        | (Zero | Pos), Neg -> Pos
        | Zero, Zero -> Zero
        | (Pos | Neg), Zero -> x 
        | Bot, _ | _, Bot -> Bot
        | Top, _ | _, Top | Pos, Pos | Neg, Neg-> Top
    )
    | AST_MULTIPLY -> (
        match x, y with
        | Pos, Pos | Neg, Neg-> Pos
        | Neg, Pos | Pos, Neg -> Neg
        | Bot, _ | _, Bot -> Bot
        | Top, _ | _, Top -> Top
        | _, Zero | Zero, _ -> Zero
    )
    | AST_DIVIDE -> (
        match x, y with
        | Pos, Pos | Neg, Neg -> Pos
        | Neg, Pos | Pos, Neg -> Neg
        | Bot, _ | _, Bot | _, Zero -> Bot
        | Top, _ | _, Top -> Top
        | Zero, _ -> Zero
    )
    | AST_MODULO -> (
        match x, y with
        | Bot, _ | _, Bot | _, Zero -> Bot
        | Zero, (Pos|Neg) -> Zero
        | _ -> Top
    )

let join x y =
    match x, y with
    | z, Bot | Bot, z -> z (* join keeps the best approximation *)
    | _ -> if x = y then x else Top (* join keeps the best approximation,
        if sign differs, then it must Top *)
let meet x y = (* dual of join *)
    match x, y with
    | z, Top | Top, z -> z
    | _ -> if x = y then x else Bot

(* We use the widening as our join. Here, widen ensures termination as the
   lattice is finite *)
let widen = join

let compare x y op = failwith "wip"
let leq x y = failwith "wip"

let bwd_unary = failwith "not implemented"
let bwd_binary = failwith "not implemented"
let narrow x y = failwith "not implemented"
