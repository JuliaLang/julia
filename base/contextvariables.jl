# This file is a part of Julia. License is MIT: https://julialang.org/license

function _uuid4 end  # to be defined by UUIDs.jl

function _ContextVar end

# `ContextVar` object itself does not hold any data (except the
# default value).  It is actually just a key into the task-local
# context storage.
"""
    ContextVar{T}

Context variable type.  This is the type of the object `var` created by
[`@contextvar var`](@ref @contextvar).  This acts as a reference to the
value stored in a task-local.  The macro `@contextvar` is the only public
API to construct this object.

!!! warning

    It is unspecified if this type is concrete or not. It may be
    changed to an abstract type and/or include more type parameters in
    the future.
"""
struct ContextVar{T}
    name::Symbol
    _module::Module
    key::UUID
    has_default::Bool
    default::T

    global _ContextVar
    _ContextVar(name, _module, key, ::Type{T}, default) where {T} =
        new{T}(name, _module, key, true, default)
    _ContextVar(name, _module, key, ::Type{T}) where {T} = new{T}(name, _module, key, false)
end

_ContextVar(name, _module, key, ::Nothing, default) =
    _ContextVar(name, _module, key, typeof(default), default)
_ContextVar(name, _module, key, ::Nothing) = _ContextVar(name, _module, key, Any)

eltype(::Type{ContextVar{T}}) where {T} = T

function show(io::IO, var::ContextVar)
    print(io, ContextVar)
    if eltype(var) !== Any && !(var.has_default && typeof(var.default) === eltype(var))
        print(io, '{', eltype(var), '}')
    end
    print(io, '(', repr(var.name))
    if var.has_default
        print(io, ", ")
        show(io, var.default)
    end
    print(io, ')')
end

function show(io::IO, ::MIME"text/plain", var::ContextVar)
    print(io, var._module, '.', var.name, " :: ContextVar")
    if get(io, :compact, false) === false
        print(io, " [", var.key, ']')
        if get(var) === nothing
            print(io, " (not assigned)")
        else
            print(io, " => ")
            show(IOContext(io, :compact => true), MIME"text/plain"(), var[])
        end
    end
end

# The primitives that can be monkey-patched to play with new context
# variable storage types:
"""
    merge_ctxvars(ctx::Union{Nothing,T}, kvs) -> ctx′:: Union{Nothing,T}

!!! warning

    This is not a public API.  This documentation is for making it easier to
    experiment with different implementations of the context variable storage
    backend, by monkey-patching it at run-time.

The first argument `ctx` is either `nothing` or a dict-like object of type `T` where
its `keytype` is `UUID` and `valtype` is `Any`.  The second argument `kvs` is an
iterable of `Pair{UUID,<:Union{Some,Nothing}}` values.  Iterable `kvs` must have
length.

If `ctx` is `nothing` and `kvs` is non-empty, `merge_ctxvars` creates a new
instance of `T`. If `ctx` is not `nothing`, it returns a shallow-copy `ctx′` of
`ctx` where `k => v` is inserted to `ctx′` for each `k => Some(v)` in `kvs`
and `k` is deleted from `ctx′` for each `k => nothing` in `kvs`.
"""
function merge_ctxvars(ctx, kvs)
    # Assumption: eltype(kvs) <: Pair{UUID,<:Union{Some,Nothing}}
    if isempty(kvs)
        return ctx
    else
        # Copy-or-create-on-write:
        vars = ctx === nothing ? Dict{UUID,Any}() : copy(ctx)
        for (k, v) in kvs
            if v === nothing
                delete!(vars, k)
            else
                vars[k] = something(v)
            end
        end
        isempty(vars) && return nothing  # should we?
        return vars
    end
end

get_task_ctxvars(t = current_task()) = t.ctxvars
set_task_ctxvars(t, ctx) = t.ctxvars = ctx
set_task_ctxvars(ctx) = set_task_ctxvars(current_task(), ctx)

struct _NoValue end

"""
    get(var::ContextVar{T}) -> Union{Some{T},Nothing}

Return `Some(value)` if `value` is assigned to `var`.  Return `nothing` if
unassigned.
"""
function get(var::ContextVar{T}) where {T}
    ctx = get_task_ctxvars()
    if ctx === nothing
        var.has_default && return Some(var.default)
        return nothing
    end
    if var.has_default
        return Some(get(ctx, var.key, var.default)::T)
    else
        y = get(ctx, var.key, _NoValue())
        y isa _NoValue || return Some(ctx[var.key]::T)
    end
    return nothing
end

"""
    getindex(var::ContextVar{T}) -> value::T

Return the `value` assigned to `var`.  Throw a `KeyError` if unassigned.
"""
function getindex(var::ContextVar{T}) where {T}
    maybe = get(var)
    maybe === nothing && throw(KeyError(var))
    return something(maybe)::T
end

setindex!(var::ContextVar, value) = set!(var, Some(value))
delete!(var::ContextVar) = set!(var, nothing)

"""
    set!(var::ContextVar, Some(value))
    set!(var::ContextVar, nothing)

Set the value of context variable `var` to `value` or delete it.
"""
function set!(var::ContextVar{T}, value::Union{Some,Nothing}) where {T}
    value = convert(Union{Some{T},Nothing}, value)
    set_task_ctxvars(merge_ctxvars(get_task_ctxvars(), (var.key => value,)))
    return var
end

"""
    genkey(__module__::Module, varname::Symbol) -> UUID

Generate a stable UUID for a context variable `__module__.\$varname`.
"""
function genkey(__module__::Module, varname::Symbol)
    pkgid = PkgId(__module__)
    if pkgid.uuid === nothing
        throw(ArgumentError(
            "Module `$__module__` is not a part of a package. " *
            "`@contextvar` can only be used inside a package.",
        ))
    end
    fullpath = push!(collect(fullname(__module__)), varname)
    if any(x -> contains(string(x), "."), fullpath)
        throw(ArgumentError(
            "Modules and variable names must not contain a dot:\n" * join(fullpath, "\n"),
        ))
    end
    return uuid5(pkgid.uuid, join(fullpath, '.'))
end

"""
    @contextvar [local|global] var[::T] [= default]

Declare a context variable named `var`.

!!! warning

    Context variables declared with `global` does not work with `Distributed`.

# Examples

Top-level context variables needs to be declared in a package:

```
module MyPackage
@contextvar cvar1
@contextvar cvar2 = 1
@contextvar cvar3::Int
end
```

Context variables can be declared in local scope by using `local` prefix:

```jldoctest
julia> function demo()
           @contextvar local x = 1
           function f()
               x[] += 1
               return x[]
           end
           return f()
       end;

julia> demo()
2
```

To use `@contextvar` in a non-package namespace like REPL, prefix the variable
with `global`:

```jldoctest global_vars
julia> @contextvar global X;

julia> X[] = 1;

julia> X[]
1
```
"""
macro contextvar(ex0)
    ex = ex0
    qualifier = :const
    if Meta.isexpr(ex, :local)
        length(ex.args) != 1 && throw(ArgumentError("Malformed input:\n$ex0"))
        ex, = ex.args
        qualifier = :local
    elseif Meta.isexpr(ex, :global)
        length(ex.args) != 1 && throw(ArgumentError("Malformed input:\n$ex0"))
        ex, = ex.args
        qualifier = :global
    end
    if Meta.isexpr(ex, :(=))
        length(ex.args) != 2 && throw(ArgumentError("Unsupported syntax:\n$ex0"))
        ex, default = ex.args
        args = Any[esc(default)]
    else
        args = []
    end
    if Meta.isexpr(ex, :(::))
        length(ex.args) != 2 && throw(ArgumentError("Malformed input:\n$ex0"))
        ex, vartype = ex.args
        pushfirst!(args, esc(vartype))
    else
        pushfirst!(args, nothing)
    end
    if !(ex isa Symbol)
        if ex === ex0
            throw(ArgumentError("Unsupported syntax:\n$ex0"))
        else
            throw(ArgumentError("""
                Not a variable name:
                $ex
                Input:
                $ex0
                """))
        end
    end
    varname = QuoteNode(ex)
    if qualifier === :const
        key = genkey(__module__, ex)
    else
        # Creating a UUID at macro expansion time because:
        # * It would be a memory leak because context variable storage can be
        #   filled with UUIDs created at run-time.
        # * Creating it at run-time is doable with function-based interface like
        #   `ContextVar(:name, default)`.
        key = _uuid4()
    end
    return Expr(
        qualifier,
        :($(esc(ex)) = _ContextVar($varname, $__module__, $key, $(args...))),
    )
end

"""
    with_context(f, var1 => value1, var2 => value2, ...)
    with_context(f, pairs)

Run `f` in a context with given values set to the context variables.  Variables
specified in this form are rolled back to the original value when `with_context`
returns.  It act like a dynamically scoped `let`.  If `nothing` is passed as
a value, corresponding context variable is cleared; i.e., it is unassigned or
takes the default value.  Use `Some(value)` to set `value` if `value` can be
`nothing`.

    with_context(f, nothing)

Run `f` in a new empty context.  All variables are rewind to the original values
when `with_context` returns.

Note that

```julia
var2[] = value2
with_context(var1 => value1) do
    @show var2[]  # shows value2
    var3[] = value3
end
@show var3[]  # shows value3
```

and

```julia
var2[] = value2
with_context(nothing) do
    var1[] = value1
    @show var2[]  # shows default (or throws)
    var3[] = value3
end
@show var3[]  # does not show value3
```

are not equivalent.
"""
function with_context(f, kvs::Pair{<:ContextVar}...)
    orig = map(((k, _),) -> k => get(k), kvs)
    set_context(kvs)
    try
        return f()
    finally
        set_context(orig)
    end
end

function with_context(f, ::Nothing)
    ctx = get_task_ctxvars()
    try
        set_task_ctxvars(nothing)
        return f()
    finally
        set_task_ctxvars(ctx)
    end
end

# Not using `set_context!` so that `snapshot` as an input makes sense.
"""
    set_context(var1 => value1, var2 => value2, ...)
    set_context(pairs)
    set_context(snapshot::ContextSnapshot)

Equivalent to `var1[] = value1`, `var2[] = value2`, and so on.  The second form
expect an iterable of pairs of context variable and values.  This function is
more efficient than setting individual context variables sequentially.  Like
[`with_context`](@ref), `nothing` value means to clear the context variable
and `Some` is always unwrapped.

A "snapshot" of the context returned from [`snapshot_context`](@ref) can
be specified as an input (the third form).

# Examples
```jldoctest
julia> @contextvar global x
       @contextvar global y;

julia> set_context(x => 1, y => 2)

julia> x[]
1

julia> y[]
2

julia> set_context([x => 10, y => 20])

julia> x[]
10

julia> y[]
20
```
"""
set_context(kvs::Pair{<:ContextVar}...) = set_context(kvs)
function set_context(kvs)
    ctx = merge_ctxvars(get_task_ctxvars(), (k.key => v for (k, v) in kvs))
    set_task_ctxvars(ctx)
    return
end

struct ContextSnapshot{T}
    vars::T
end

# TODO: Do we need to implement `Dict{ContextVar}(::ContextSnapshot)`?
# This requires storing UUID-to-ContextVar mapping somewhere.

"""
    snapshot_context() -> snapshot::ContextSnapshot

Get a snapshot of a context that can be passed to [`reset_context`](@ref) to
rewind all changes in the context variables.
"""
snapshot_context() = ContextSnapshot(get_task_ctxvars())

"""
    reset_context(snapshot::ContextSnapshot)

Rest the entire context of the current task to the state at which `snapshot`
is obtained via [`snapshot_context`](@ref).
"""
reset_context(snapshot::ContextSnapshot) = set_task_ctxvars(snapshot.vars)
set_context(snapshot::ContextSnapshot) = set_context(snapshot.vars)
