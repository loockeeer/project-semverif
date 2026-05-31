open Frontend
open Frontend.AbstractSyntax
open Congruence
open Sign

type bounds =  | Ninf | Pinf | Integer of Z.t
type t = Top | Bot | Interval of bounds * bounds

let bound_to_string b =
    match b with
    | Ninf -> "-∞"
    | Pinf -> "+∞"
    | Integer x -> Z.to_string x

let pp fmt x = Format.fprintf fmt "%s" (
    match x with
    | Top -> "[-∞;+∞]"
    | Bot -> "Ø"
    | Interval(istart, iend) -> Printf.sprintf "[%s;%s]" (bound_to_string istart) (bound_to_string iend)
) 

let top = Top
let bottom = Bot
let is_bottom = (=) Bot

let const c = Interval ((Integer c),(Integer c))
let rand x y = Interval ((Integer (min x y)),(Integer (max x y)))

let bound_uminus b =
    match b with
    | Ninf -> Pinf
    | Pinf -> Ninf
    | Integer x -> Integer (Z.neg x)

let unary x op =
    match op with
    | AST_UNARY_PLUS -> x
    | AST_UNARY_MINUS -> 
            (* This is a symetry relative to 0 *)
            match x with
            | Top | Bot -> x
            | Interval (istart, iend) -> Interval((bound_uminus iend), (bound_uminus istart))
                        
let binary x y op = x

let join x y = (* interval union *)
    match x, y with
    | Top, _ | _, Top -> Top
    | z, Bot | Bot, z -> z
    | Interval (xs, xe), Interval (ys, ye) ->
            Interval ((min xs ys), (max xe ye))

let lt_bounds x y =
    match x, y with
    | _, Pinf | Ninf, _ -> true
    | Integer x, Integer y -> x < y
    | _ -> false
let le_bounds x y =
    match x, y with
    | _, Pinf | Ninf, _ -> true
    | Integer x, Integer y -> x <= y
    | _ -> false
let rec meet x y = (* interval intersection *)
    match x, y with
    | Bot, _ | _, Bot -> Bot
    | z, Top | Top, z -> z
    | Interval (xs, xe), Interval (ys, ye) ->
            if lt_bounds ys xs then meet y x
            else
                if le_bounds ys xe  then
                    Interval (ys, xe)
                else Bot

let widen x y = join x y

let compare x y op = (x, y)
let leq x y = true

let bwd_unary x _ _ = x

let bound_sign x =
    match x with
    | Integer x when x = Z.zero -> `Zero
    | Integer x when x > Z.zero -> `Pos
    | Pinf -> `Pos
    | _ -> `Neg

let contains_zero x =
    match x with
    | Top -> true
    | Bot -> false
    | Interval (istart, iend) ->
            bound_sign istart <> bound_sign iend

let bwd_binary x y op r =
    match op with
    | AST_PLUS -> meet x (binary r y AST_MINUS), meet y (binary r x AST_MINUS)
    | AST_MINUS -> meet x (binary r y AST_PLUS), meet y (binary r x AST_PLUS)
    | AST_DIVIDE -> meet x (binary r y AST_MULTIPLY), meet y (binary r x AST_MULTIPLY)
    | AST_MODULO -> (x, x)
    | AST_MULTIPLY -> 
            let left = match y, r with
                    | _ when contains_zero y && contains_zero r -> x
                    | _ when contains_zero y -> bottom
                    | _ -> meet x (binary r y AST_DIVIDE)
            in
            let right = match x, r with
            | _ when contains_zero x && contains_zero r -> y
            | _ when contains_zero x -> bottom
            | _ -> meet y (binary r x AST_DIVIDE)
            in
            left, right

let narrow _ _ = failwith "not implemented"
