"""
    @opaque (args...) -> body

Marks a given closure as "opaque". Opaque closures capture the
world age of their creation (as opposed to their invocation).
This allows for more aggressive optimization of the capture
list, but trades off against the ability to inline opaque
closures at the call site, if their creation is not statically
visible.

!!! warning
    This interface is experimental and subject to change or removal without notice.
"""
macro opaque(ex)
    esc(Expr(:opaque_closure, ex))
end
