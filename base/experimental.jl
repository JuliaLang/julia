# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    Experimental

!!! warning
    Types, methods, or macros defined in this module are experimental and subject
    to change and will not have deprecations. Caveat emptor.
"""
module Experimental

using Base: Threads, sync_varname

"""
    Const(A::Array)

Mark an Array as constant/read-only. The invariant guaranteed is that you will not
modify an Array (through another reference) within an `@aliasscope` scope.

!!! warning
    Experimental API. Subject to change without deprecation.
"""
struct Const{T,N} <: DenseArray{T,N}
    a::Array{T,N}
end

Base.IndexStyle(::Type{<:Const}) = IndexLinear()
Base.size(C::Const) = size(C.a)
Base.axes(C::Const) = axes(C.a)
@eval Base.getindex(A::Const, i1::Int) =
    (Base.@_inline_meta; Core.const_arrayref($(Expr(:boundscheck)), A.a, i1))
@eval Base.getindex(A::Const, i1::Int, i2::Int, I::Int...) =
  (Base.@_inline_meta; Core.const_arrayref($(Expr(:boundscheck)), A.a, i1, i2, I...))

"""
    @aliasscope expr

Allows the compiler to assume that all `Const`s are not being modified through stores
within this scope, even if the compiler can't prove this to be the case.

!!! warning
    Experimental API. Subject to change without deprecation.
"""
macro aliasscope(body)
    sym = gensym()
    quote
        $(Expr(:aliasscope))
        $sym = $(esc(body))
        $(Expr(:popaliasscope))
        $sym
    end
end


function sync_end(refs)
    local c_ex
    defined = false
    t = current_task()
    cond = Threads.Condition()
    lock(cond)
    nremaining = length(refs)
    for r in refs
        schedule(Task(()->begin
            try
                wait(r)
                lock(cond)
                nremaining -= 1
                nremaining == 0 && notify(cond)
                unlock(cond)
            catch e
                lock(cond)
                notify(cond, e; error=true)
                unlock(cond)
            end
        end))
    end
    wait(cond)
    unlock(cond)
end

"""
    Experimental.@sync

Wait until all lexically-enclosed uses of `@async`, `@spawn`, `@spawnat` and `@distributed`
are complete, or at least one of them has errored. The first exception is immediately
rethrown. It is the responsibility of the user to cancel any still-running operations
during error handling.

!!! Note
    This interface is experimental and subject to change or removal without notice.
"""
macro sync(block)
    var = esc(sync_varname)
    quote
        let $var = Any[]
            v = $(esc(block))
            sync_end($var)
            v
        end
    end
end

"""
    Experimental.@optlevel n::Int

Set the optimization level (equivalent to the `-O` command line argument)
for code in the current module. Submodules inherit the setting of their
parent module.

Supported values are 0, 1, 2, and 3.

The effective optimization level is the minimum of that specified on the
command line and in per-module settings.
"""
macro optlevel(n::Int)
    return Expr(:meta, :optlevel, n)
end

end
