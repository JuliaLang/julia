@noinline function (y::Core.OpaqueClosure{A, R})(args...) where {A,R}
    typeassert(args, A)
    ccall(y.fptr1, Any, (Any, Ptr{Any}, Int), y, Any[args...], length(args))::R
end

"""
    _opaque_closure(argt::Type{<:Tuple}, lb::Type, ub::Type, source::CodeInfo, captures...)

Create a new OpaqueClosure taking arguments specified by the types `argt`. When called,
this opaque closure will execute the source specified in `source`. The `lb` and `ub`
arguments constrain the return type of the opaque closure. In particular, any return
value of type `Core.OpaqueClosure{argt, R} where lb<:R<:ub` is semantically valid. If
the optimizer runs, it may replace `R` by the narrowest possible type inference
was able to determine. To guarantee a particular value of `R`, set lb===ub.
"""
Core._opaque_closure

function show(io::IO, oc::Core.OpaqueClosure{A, R}) where {A, R}
    types = map(@nospecialize(T)->Expr(:(::), T), A.parameters)
    show_enclosed_list(io, '(', types, ',', ')', 0)
    print(io, "::", R)
    print(io, "->◌")
end


# @opaque macro goes here
