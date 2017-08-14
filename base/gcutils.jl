# This file is a part of Julia. License is MIT: https://julialang.org/license

==(w::WeakRef, v::WeakRef) = isequal(w.value, v.value)
==(w::WeakRef, v) = isequal(w.value, v)
==(w, v::WeakRef) = isequal(w, v.value)

function finalizer(@nospecialize(o), @nospecialize(f))
    if isimmutable(o)
        error("objects of type ", typeof(o), " cannot be finalized")
    end
    ccall(:jl_gc_add_finalizer_th, Void, (Ptr{Void}, Any, Any),
          Core.getptls(), o, f)
end
function finalizer(o::T, f::Ptr{Void}) where T
    @_inline_meta
    if isimmutable(T)
        error("objects of type ", T, " cannot be finalized")
    end
    ccall(:jl_gc_add_ptr_finalizer, Void, (Ptr{Void}, Any, Ptr{Void}),
          Core.getptls(), o, f)
end

finalize(@nospecialize(o)) = ccall(:jl_finalize_th, Void, (Ptr{Void}, Any,),
                                   Core.getptls(), o)

gc(full::Bool=true) = ccall(:jl_gc_collect, Void, (Int32,), full)
gc_enable(on::Bool) = ccall(:jl_gc_enable, Int32, (Int32,), on) != 0
