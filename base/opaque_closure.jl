@noinline function (y::Core.OpaqueClosure{A, R})(args...) where {A,R}
    typeassert(args, A)
    ccall(y.fptr1, Any, (Any, Ptr{Any}, Int), y, Any[args...], length(args))::R
end

function show(io::IO, oc::Core.OpaqueClosure{A, R}) where {A, R}
    types = map(@nospecialize(T)->Expr(:(::), T), A.parameters)
    show_enclosed_list(io, '(', types, ',', ')', 0)
    print(io, "::", R)
    print(io, "->◌")
end

"""
    @opaque (args...)->...

Marks a given closure as "opaque". Opaque closures capture the
world age of their creation (as opposed to their invocation).
This allows for more aggressive optimization of the capture
list, but trades off against the ability to inline opaque
closures at the call site, if their creation is not statically
visible.
"""
macro opaque(ex)
    esc(Expr(:opaque_closure, ex))
end
