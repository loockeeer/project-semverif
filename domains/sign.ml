open Frontend
open Frontend.AbstractSyntax

type t = Zero | Pos | Neg | Top | Bot

let pp fmt x = Format.fprintf fmt "%s" (
    match x with
    | Top -> "⊤"
    | Bot -> "⊥"
    | Zero -> "0"
    | Pos -> "+"
    | Neg -> "-"
) 

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

let compare x y op = (x, y)
let leq x y = match x, y with
| Bot, _ | _, Top -> true (* [Bot] is included in everything, and everything
is included in [Top], by definition *)
| _ -> x = y (* Since the concrete domains associated with [Pos], [Neg] and
[Zero] are all disjoint, they are never included in one another *)

let bwd_unary x _ _ = x
let bwd_binary x y op r =
    match op with
    | AST_PLUS -> meet x (binary r y AST_MINUS), meet y (binary r x AST_MINUS)
    | AST_MINUS -> meet x (binary r y AST_PLUS), meet y (binary r x AST_PLUS)
    | AST_DIVIDE -> meet x (binary r y AST_MULTIPLY), meet y (binary r x AST_MULTIPLY)
    | AST_MODULO -> (x, x)
    | AST_MULTIPLY -> 
            let left = match y, r with
                    | Zero, Zero -> x
                    | Zero, _ -> bottom
                    | _ -> meet x (binary r y AST_DIVIDE)
            in
            let right = match x, r with
            | Zero, Zero -> y
            | Zero, _ -> bottom
            | _ -> meet y (binary r x AST_DIVIDE)
            in
            left, right 
let narrow _ _ = failwith "not implemented"
