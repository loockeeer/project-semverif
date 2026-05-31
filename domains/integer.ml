open Frontend
open Frontend.AbstractSyntax

type t = Top | Bot | Integer of Z.t

let pp fmt x = Format.fprintf fmt "%s" (
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
    | Top, Integer(n) | Integer(n), Top when n = Z.zero && op = AST_MULTIPLY -> Integer (Z.zero)
    | Top, _ | _, Top -> Top
    | _, Integer(n) when n = Z.zero && (op = AST_DIVIDE || op = AST_MODULO) -> bottom
    | Integer x, Integer y ->(
        match op with
        | AST_PLUS -> Integer (Z.add x y)
        | AST_MINUS -> Integer (Z.sub x y)
        | AST_DIVIDE -> if y = Z.zero then bottom else Integer (Z.div x y)
        | AST_MULTIPLY -> Integer (Z.mul x y)
        | AST_MODULO -> if y = Z.zero then bottom else Integer (Z.(mod) x y))

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
let compare x y op =
    match op with
    | AST_EQUAL -> (
        match x, y with
        | Bot, _ | _, Bot -> (Bot, Bot)
        | (Integer _) as z, _ | _, ((Integer _) as z) -> (z, z)
        | Top, Top -> (Top, Top)
    )
    | AST_NOT_EQUAL -> (
        match x, y with
        | Top, Top -> (Top, Top)
        | Bot, _ | _, Bot -> (Bot, Bot)
        | Top, Integer _ | Integer _, Top -> (x, y)
        | Integer _, Integer _ when x <> y -> (x, y)
        | _ -> (Bot, Bot)
    )
    | _ -> match x, y with
    | Top, Top -> (Top, Top)
    | Bot, _ | _, Bot -> (Bot, Bot)
    | Top, Integer _ | Integer _, Top -> (x, y)
    | Integer x, Integer y ->
            let op = Z.(match op with
            | AST_GREATER -> gt
            | AST_LESS -> lt
            | AST_GREATER_EQUAL -> geq 
            | AST_LESS_EQUAL -> leq
            | _ -> failwith "unreachable")
            in
            if op x y then (Integer x, Integer y)
            else (Bot, Bot)

let leq x y = (* almost the same as for the sign domain *)
    match x, y with
    | Bot, _ | _, Top -> true
    | _ -> if x = y then true else false

let bwd_unary x _ _ = x
let bwd_binary x y op r =
    match op with
    | AST_PLUS -> meet x (binary r y AST_MINUS), meet y (binary r x AST_MINUS)
    | AST_MINUS -> meet x (binary r y AST_PLUS), meet y (binary r x AST_PLUS)
    | AST_DIVIDE -> meet x (binary r y AST_MULTIPLY), meet y (binary r x AST_MULTIPLY)
    | AST_MODULO -> (x, x)
    | AST_MULTIPLY -> 
            let left = match y, r with
                    | Integer a, Integer b when a = Z.zero && b = Z.zero -> x
                    | Integer a, _ when a = Z.zero -> bottom
                    | _ -> meet x (binary r y AST_DIVIDE)
            in
            let right = match x, r with
            | Integer a, Integer b when a = Z.zero && b = Z.zero -> y
            | Integer a, _ when a = Z.zero -> bottom
            | _ -> meet y (binary r x AST_DIVIDE)
            in
            left, right

let narrow _ _ = failwith "not implemented"
