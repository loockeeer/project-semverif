# Semverif project report

## State of the project

We implemented the iterator for forward analysis only, and implemented the
- sign domain;
- constants domain;
- interval domain (wip);
- polyhedral domain(s);

All tests pass for the constants and polyhedral domains.

## Implementation details

We modified the iterator's signature as suggested by the subject :
```ocaml
module type Iterator = sig
    module Make : functor (DOMAIN) -> sig
        let iterate : cfg -> unit
    end
end
```

and added a `MakeForwardOnly` functor to `domain.ml` :
```ocaml
module type Domain = sig
    module MakeForwardOnly : functor (VARS) -> functor (VALUE_DOMAIN) -> DOMAIN
end
```

The polyhedral domain is, too, a functor.

Files `sign.ml`, `interval.ml`, `integer.ml`, `congruence.ml`, `reducedProduct.ml` all obey the `VALUE_DOMAIN` signature, so that
they can be passed to the `Domain.MakeForwardOnly _` functor.

## Iterator

The iterator is made of three parts :
- `find_loop_nodes`, which finds cycles in the program and outputs a set of nodes on which widenings will be necessary to ensure termination
- `iter_arc`, which interprets the program's instructions and interfaces with the provided domain
- `loop`, which is the worklist fixpoint algorithm. During its execution, it keeps a mapping of nodes to a domain always satisfied at their position.
during its execution, the algorithm refines the mapping so that each domain becomes a domain invariant.

## MakeForwardOnly

This functor is used by all «trivial» domains in the project, and hence was quite important to get to work.

We implemented a few utility functions, such as `get_value : t -> int_expr -> Vd.t` that interprets expressions, `prop_not : bool_expr -> bool_expr`, that propagates the
negation of a proposition using logic rules, and finally `bwd : t -> int_expr -> Vd.t -> t`, that outputs an extension of the domain passed as the first argument such
that the int expression passed as a second argument evaluates to the abstract value passed as a third argument. This effectively constitutes a backward analysis, which
is used to model guards properly.


## Implementation details for the different value domains
### Integer domain (or constants domain)
This one is the «interpreter» abstract domain; i.e. the concrete domain directly. Since the syntax allows for `rand` and `brand` constructs, we do need a
`Top` element that represents uncertainty in the value.
We chose `type t = Top | Bot | Integer of Z.t`.

The functions here are quite trivial, except for `compare` that required more work;

### Sign domain
Nothing to say here, we chose `type t = Pos | Neg | Zero` as it makes sense.

### Interval domain

We chose this representation of the intervals : `type t = Top | Bot | Interval of bounds * bounds` where `type bounds = Pinf | Ninf | Integer of Z.t`. Where `Pinf` and `Ninf` represent
positive and negative infinity, respectively.

Note that there is an overlap between `Top` and other constructs (`Interval Ninf Pinf` semantically equals `Top`), which doesn't pose any problems later on.

### Polyhedral domain(s)
We use the APRON library to represent the polyhedral domain. Arithmetic operations are transformed 
into linear expressions when possible, which allows to express constraints in the domain.

One difficulty was writing the domain without being able to test it, as the iterator was not finished yet. Added to this,
the domain is very complex and when expressions are not linear, deciding on the transformation is not trivial.
