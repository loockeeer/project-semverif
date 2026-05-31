module type BACKEND = sig
	type man
	val manager : man Apron.Manager.t
end

module Make :
	functor (_ : Domain.VARS) (_ : ValueDomain.VALUE_DOMAIN) (_ : BACKEND) ->
		Domain.DOMAIN
