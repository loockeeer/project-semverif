open Frontend
open Frontend.AbstractSyntax
open Congruence
open Interval

type t = Congruence.t * Interval.t

let pp fmt x = Format.fprintf fmt "(%a, %a)" Congruence.pp (fst x) Interval.pp (snd x)

let top = (Congruence.top, Interval.top)
let bottom = (Congruence.bottom, Interval.bottom)

let const c = (Congruence.const c, Interval.const c)
let rand x y = (Congruence.rand x y, Interval.rand x y)

let unary x op = (Congruence.unary (fst x) op, Interval.unary (snd x) op)
let binary x y op = (Congruence.binary (fst x) (fst y) op, Interval.binary (snd x) (snd y) op )
let p (c,i) = failwith "wip"
let is_bottom (c,i) = Congruence.is_bottom c || Interval.is_bottom i

let join (c1,i1) (c2,i2) = p (Congruence.join c1 c2, Interval.join i1 i2)
let meet (c1,i1) (c2,i2) = p (Congruence.meet c1 c2, Interval.meet i1 i2)
let widen (c1,i1) (c2,i2) = (Congruence.widen c1 c2, Interval.widen i1 i2)

let compare (c1,i1) (c2,i2) op = failwith "wip"
let leq (c1,i1) (c2,i2) = Congruence.leq c1 c2 && Interval.leq i1 i2

let bwd_unary x op r = failwith "not implemented"

let bwd_binary x y op r = failwith "not implemented"

let narrow x y = failwith "not implemented"
