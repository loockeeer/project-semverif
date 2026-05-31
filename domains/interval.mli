type bounds = Ninf | Pinf | Integer of Z.t
type t = Top | Bot | Interval of bounds * bounds

include ValueDomain.VALUE_DOMAIN with type t := t
