open Frontend
open Frontend.AbstractSyntax
open Congruence
open Interval

type t = Congruence.t * Interval.t

let pp fmt x = Format.fprintf fmt "(%a, %a)" Congruence.pp (fst x) Interval.pp (snd x)

let top = (Congruence.top, Interval.top)
let bottom = (Congruence.bottom, Interval.bottom)

let positive_mod x m =
	let r = Z.erem x m in
	if Z.sign r < 0 then Z.add r m else r

let reduce (c, i) =
	match c, i with
	| _, _ when Congruence.is_bottom c || Interval.is_bottom i -> bottom
	| Congruence.Modulo(a, b), _ when Z.equal b Z.zero ->
		let i' = Interval.meet i (Interval.const a) in
		if Interval.is_bottom i' then bottom else (Congruence.const a, i')
	| _, Interval.Interval(Interval.Integer lo, Interval.Integer hi) ->
		if Z.gt lo hi then bottom
		else (
			match c with
			| Congruence.Modulo(a, b) when not (Z.equal b Z.zero) ->
				let m = Z.abs b in
				let first = Z.add lo (positive_mod (Z.sub a lo) m) in
				let last = Z.sub hi (positive_mod (Z.sub hi a) m) in
				if Z.gt first hi || Z.lt last lo then bottom
				else
					let i' = Interval.meet i (Interval.rand first last) in
					if Interval.is_bottom i' then bottom
					else if Z.equal first last then (Congruence.const first, i')
					else (c, i')
			| _ -> (c, i)
		)
	| _, _ -> (c, i)

let p x =
	let x' = reduce x in
	if x' = x then x else reduce x'

let const c = p (Congruence.const c, Interval.const c)
let rand x y = p (Congruence.rand x y, Interval.rand x y)

let unary x op = p (Congruence.unary (fst x) op, Interval.unary (snd x) op)
let binary x y op = p (Congruence.binary (fst x) (fst y) op, Interval.binary (snd x) (snd y) op )
let is_bottom (c,i) = Congruence.is_bottom c || Interval.is_bottom i

let join (c1,i1) (c2,i2) = p (Congruence.join c1 c2, Interval.join i1 i2)
let meet (c1,i1) (c2,i2) = p (Congruence.meet c1 c2, Interval.meet i1 i2)
let widen (c1,i1) (c2,i2) = (Congruence.widen c1 c2, Interval.widen i1 i2)

let compare (c1,i1) (c2,i2) op =
	let c1', c2' = Congruence.compare c1 c2 op in
	let i1', i2' = Interval.compare i1 i2 op in
	(p (c1', i1'), p (c2', i2'))
let leq (c1,i1) (c2,i2) = Congruence.leq c1 c2 && Interval.leq i1 i2

let bwd_unary x op r = p (Congruence.bwd_unary (fst x) op (fst r), Interval.bwd_unary (snd x) op (snd r))

let bwd_binary x y op r =
	let c_left, c_right = Congruence.bwd_binary (fst x) (fst y) op (fst r) in
	let i_left, i_right = Interval.bwd_binary (snd x) (snd y) op (snd r) in
	(p (c_left, i_left), p (c_right, i_right))

let narrow x y = failwith "not implemented"
