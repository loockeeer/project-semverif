type t = Bot | Modulo of Z.t * Z.t

include ValueDomain.VALUE_DOMAIN with type t := t
