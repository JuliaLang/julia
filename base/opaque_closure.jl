function show(io::IO, oc::Core.OpaqueClosure{A, R}) where {A, R}
    show_tuple_as_call(io, Symbol(""), A; hasfirst=false)
    print(io, "::", R)
    print(io, "->◌")
end

function show(io::IO, ::MIME"text/plain", oc::Core.OpaqueClosure{A, R}) where {A, R}
    show(io, oc)
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
