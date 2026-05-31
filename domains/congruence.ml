open Frontend
open Frontend.AbstractSyntax

type t = Bot | Modulo of Z.t * Z.t (* x is Modulo(a, b) if x = a (mod b) *)

let pp fmt x =
    match x with
    | Bot -> Format.fprintf fmt "⊥@."
    | Modulo(a, b) -> Format.fprintf fmt "%s (mod %s)@." (Z.to_string a) (Z.to_string b)

let top = Modulo(Z.zero, Z.one)
let bottom = Bot
let is_bottom = (=) Bot

let is_top = function
    | Modulo(_, b) -> Z.equal b Z.one
    | Bot -> false

let member c = function
    | Bot -> false
    | Modulo(a, b) when Z.equal b Z.zero -> Z.equal c a
    | Modulo(a, b) -> Z.equal (Z.rem (Z.sub c a) (Z.abs b)) Z.zero

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
        | Modulo(x1, x2), Modulo(y1, y2) ->
            if is_top x || is_top y then top
            else if Z.equal x2 Z.zero && Z.equal y2 Z.zero then Modulo(Z.mul x1 y1, Z.zero)
            else if Z.equal x2 Z.zero then
                if Z.equal x1 Z.zero then const Z.zero else top
            else if Z.equal y2 Z.zero then
                if Z.equal y1 Z.zero then const Z.zero else top
            else top
    )
    | AST_DIVIDE -> (
        match x, y with
        | Bot, _ | _, Bot -> Bot
        | Modulo(x1, x2), Modulo(y1, y2) ->
            if Z.equal y2 Z.zero then if Z.equal y1 Z.zero then bottom else Modulo(Z.div x1 y1, Z.zero)
            else if Z.equal x1 Z.zero then const Z.zero
            else top
    )
    | AST_MODULO -> (
        match x, y with
        | Bot, _ | _, Bot -> Bot
        | Modulo(x1, x2), Modulo(y1, y2) ->
            if Z.equal y2 Z.zero then bottom
            else if Z.equal x2 Z.zero && Z.equal y2 Z.zero then Modulo(Z.rem x1 y1, Z.zero)
            else if Z.equal x1 Z.zero then const Z.zero
            else top
    )

let join x y =
    match x, y with
    | Bot, z | z, Bot -> z
    | _ when x = y -> x
    | _ when is_top x || is_top y -> top
    | Modulo(c, b), z when Z.equal b Z.zero -> if member c z then z else top
    | z, Modulo(c, b) when Z.equal b Z.zero -> if member c z then z else top
    | _ -> top

let meet x y =
    match x, y with
    | Bot, _ | _, Bot -> Bot
    | z, t when is_top z -> t
    | z, t when is_top t -> z
    | _ when x = y -> x
    | Modulo(c, b), z when Z.equal b Z.zero -> if member c z then Modulo(c, b) else Bot
    | z, Modulo(c, b) when Z.equal b Z.zero -> if member c z then Modulo(c, b) else Bot
    | _ -> top
let widen = join

let compare x y op =
    match x, y with
    | Bot, _ | _, Bot -> (Bot, Bot)
    | Modulo(x1, x2), Modulo(y1, y2) ->
        if Z.equal x2 Z.zero && Z.equal y2 Z.zero then
            let holds =
                match op with
                | AST_EQUAL -> Z.equal x1 y1
                | AST_NOT_EQUAL -> not (Z.equal x1 y1)
                | AST_LESS -> Z.compare x1 y1 < 0
                | AST_LESS_EQUAL -> Z.compare x1 y1 <= 0
                | AST_GREATER -> Z.compare x1 y1 > 0
                | AST_GREATER_EQUAL -> Z.compare x1 y1 >= 0
            in
            if holds then (x, y) else (Bot, Bot)
        else (x, y)

let leq x y =
    match x, y with
    | Bot, _ -> true
    | _, z when is_top z -> true
    | _ when x = y -> true
    | Modulo(c, b), z when Z.equal b Z.zero -> member c z
    | _ -> false

let bwd_unary x op r = meet x (unary r op)

let bwd_binary x y op r =
    match op with
    | AST_PLUS -> meet x (binary r y AST_MINUS), meet y (binary r x AST_MINUS)
    | AST_MINUS -> meet x (binary r y AST_PLUS), meet y (binary r x AST_PLUS)
    | AST_DIVIDE -> meet x (binary r y AST_MULTIPLY), meet y (binary r x AST_MULTIPLY)
    | AST_MODULO -> (x, x)
    | AST_MULTIPLY ->
        let left =
            match y, r with
            | Modulo(a, ba), Modulo(b, br) when Z.equal ba Z.zero && Z.equal br Z.zero && Z.equal a Z.zero && Z.equal b Z.zero -> x
            | Modulo(a, ba), _ when Z.equal ba Z.zero && Z.equal a Z.zero -> if member Z.zero r then x else bottom
            | _ -> meet x (binary r y AST_DIVIDE)
        in
        let right =
            match x, r with
            | Modulo(a, ba), Modulo(b, br) when Z.equal ba Z.zero && Z.equal br Z.zero && Z.equal a Z.zero && Z.equal b Z.zero -> y
            | Modulo(a, ba), _ when Z.equal ba Z.zero && Z.equal a Z.zero -> if member Z.zero r then y else bottom
            | _ -> meet y (binary r x AST_DIVIDE)
        in
        left, right
let narrow _ _ = failwith "not implemented"
