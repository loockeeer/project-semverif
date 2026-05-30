open Frontend
open Frontend.AbstractSyntax
open Congruence
open Interval

type t = unit

let pp fmt x = failwith "wip"

let top = ()
let bottom = ()
let is_bottom _ = failwith "wip"

let const c = failwith "wip"
let rand x y = failwith "wip"

let unary x op = failwith "wip"
let binary x y op = failwith "wip"

let join x y = failwith "wip"
let meet x y = failwith "wip"
let widen x y = failwith "wip"

let compare x y = failwith "wip"
let leq x y = failwith "wip"

let bwd_unary _ _ _ = failwith "not implemented"
let bwd_binary _ _ _ = failwith "not implemented"
let narrow _ _ = failwith "not implemented"
