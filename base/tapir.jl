# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    Tapir

Optional parallelism API.

This module provides two public API `Tapir.@sync` and `Tapir.@spawn` for
denoting tasks that _may or may not_ run in parallel.
"""
module Tapir

#####
##### Julia-Tapir Runtime
#####

const TaskGroup = Channel{Any}

taskgroup() = Channel{Any}(Inf)::TaskGroup

function spawn!(tasks::TaskGroup, @nospecialize(f))
    t = Task(f)
    t.sticky = false
    schedule(t)
    push!(tasks, t)
    return nothing
end

# Can we make it more efficient using the may-happen parallelism property
# (i.e., the lack of concurrent synchronizations)?
sync!(tasks::TaskGroup) = Base.sync_end(tasks)

mutable struct UndefableRef{T}
    set::Bool
    x::T
    UndefableRef{T}() where {T} = new{T}(false)
end

function Base.setindex!(ref::UndefableRef, x)
    ref.x = x
    ref.set = true
    return ref
end

function Base.getindex(ref::UndefableRef)
    ref.set || not_set_error(ref)
    return ref.x
end

@noinline function not_set_error(::UndefableRef{T}) where {T}
    error("variable of type `$T` is not set")
end

abstract type TapirRaceError <: Exception end

struct RacyStoreError <: TapirRaceError
    value::Any
end

struct RacyLoadError <: TapirRaceError
    value::Any
end

@noinline racy_store(@nospecialize(v)) = throw(RacyStoreError(v))
@noinline racy_load(@nospecialize(v)) = throw(RacyLoadError(v))

function Base.showerror(io::IO, e::RacyStoreError)
    print(io, "Tapir: racy store of a value that may be read concurrently: ")
    show(io, e.value)
end

function Base.showerror(io::IO, e::RacyLoadError)
    print(io, "Tapir: racy load of a value that may be read concurrently: ")
    show(io, e.value)
end

@noinline function warn_race()
    bt = backtrace()

    # Go out of this function's stack:
    i = firstindex(bt)
    while i <= lastindex(bt)
        s, = Base.StackTraces.lookup(bt[i])
        if s.linfo isa Core.MethodInstance && s.linfo.specTypes === Tuple{typeof(warn_race)}
            i += 1
        elseif !s.from_c
            i += 1
        else
            break
        end
    end

    # Find the first non-C function:
    _module = Tapir
    if i <= lastindex(bt)
        ptr = bt[i]
        s, = Base.StackTraces.lookup(ptr)
        if s.linfo isa Core.MethodInstance
            _module = s.linfo.def.module
        end
        id = ptr
        file = String(s.file)
        line = s.line
    else
        id = :default_warn_race_id
        file = ""
        line = 0
    end

    @warn(
        raw"""
        Tapir: Detected racy updates of variable(s). Use the output variable syntax
            $output = value
        to explicitly denote the output variables or use
            local variable
        inside `Tapir.@spawn` to declare that `variable` is used only locally in a task.

        See more information in `Tapir.@spawn` documentation.
        """,
        _module = _module,
        _file = file,
        _line = line,
        _id = id,
        maxlog = 1,
    )
end

@noinline function escaping_task_output_error(name = nothing)::Union{}
    @nospecialize
    msg = "Tapir: escaping task output variable"
    if name !== nothing
        msg = "$msg: $name"
    end
    throw(ConcurrencyViolationError(msg))
end

#####
##### Julia-Tapir Frontend
#####

"""
    Tapir._load(x, name::Symbol) -> x

[INTERNAL] A placeholder for denoting where the task output variables named
`name` should be loaded.

The task output syntax

    Tapir.@sync begin
        ...
        Tapir.@spawn \$output = rhs
        ...
    end

is lowered to an AST equivalent to

    local slot
    Tapir.@sync begin
        ...
        Tapir.@spawn slot = rhs
        ...
    end
    output = Tapir._load(slot, :output)

The function `_load` returns the first argument as-is. This lets the inference
to support task output without any change (given that the source program does
not have a race).

The function `_load` treated specially during slot2reg; i.e., the slots
specified as the first argument are _not_ promoted to SSA registers (ref
`task_output_slots`).  These leftover slots are transformed to PhiC and Upsilon
nodes inside the `lower_tapir_output!` pass run just after slot2reg.  The PhiC
and Upsilon nodes that cross task boundaries are finally lowered to
`Tapir.UndefableRef` in `lower_tapir_task!`.  (Note: PhiC and Upsilon nodes
translate to stack memory in LLVM.  So, this representation may also be useful
for Tapir/LLVM integration.)

* TODO: Add an IR node that directly represents `_load`? It would reduce the
  transformation inside the macro.
* TODO: Support conditionally defined task output.

A possible extension is to forbid

    Tapir.@sync begin
        Tapir.@spawn if p()
            ...
            \$a = rhs1
        end
        if q()
            ...
            \$a = rhs2
        end
    end

and

    Tapir.@sync for x in xs
        Tapir.@spawn if p(x)
            \$a = rhs1
        end
    end

_by default_ so that we can detect this possibly-racy pattern. It should be
possible to opt-in the above pattern by a special syntax such as
`\$(a::@norace) = rhs`; i.e., the user declares that at most one task executes
this line of code.
"""
_load(x, ::Symbol) = x

macro syncregion()
    Expr(:syncregion)
end

macro spawnin(token, expr)
    Expr(:spawn, esc(token), esc(Expr(:block, __source__, expr)))
end

macro sync_end(token)
    Expr(:sync, esc(token))
end

macro loopinfo(args...)
    Expr(:loopinfo, args...)
end

isassignment(ex) = Meta.isexpr(ex, :(=)) && length(ex.args) > 1

function output_vars(ex::Expr)
    outputs = Dict{Symbol,Symbol}()

    function lhs_output_vars(ex)
        if Meta.isexpr(ex, :$, 1)
            v, = ex.args
            if v isa Symbol
                slot = get!(outputs, v) do
                    gensym("output_$(v)_")
                end
                return slot
            end
        elseif Meta.isexpr(ex, :tuple)
            return Expr(:tuple, map(lhs_output_vars, ex.args)...)
        end
        return ex
    end

    process(x) = x
    function process(ex::Expr)
        if isassignment(ex)
            a1 = lhs_output_vars(ex.args[1])
            rest = map(process, ex.args[2:end])
            return Expr(ex.head, a1, rest...)
        elseif Meta.isexpr(ex, :tuple) && (i = findfirst(isassignment, ex.args)) !== nothing
            # Handle: a, b, c = d, e, f
            args1 = map(lhs_output_vars, ex.args[1:i-1])        # a, b
            arg2 = lhs_output_vars(ex.args[i].args[1])          # c
            args3 = map(process, ex.args[i].args[2:end])        # d
            args4 = map(process, ex.args[i+1:end])              # e, f
            return Expr(:tuple, args1..., Expr(:(=), arg2, args3...), args4...)
        elseif ex.head in (:meta, :inbounds)
            return ex
        else
            return Expr(ex.head, map(process, ex.args)...)
        end
    end

    return outputs, process(ex)
end

const tokenname = gensym(:token)

"""
    Tapir.@sync block
"""
macro sync(block)
    var = esc(tokenname)
    block = macroexpand(__module__, block)  # for nested @sync
    block = Expr(:block, __source__, block)
    dict, block = output_vars(block)
    outputs = sort!(collect(dict))
    header = [:(local $(esc(slot))) for (_, slot) in outputs]
    footer =
        [:($(esc(v)) = _load($(esc(slot)), $((QuoteNode(v))))) for (v, slot) in outputs]
    quote
        $(header...)
        let $var = @syncregion()
            local ans
            try
                ans = $(esc(block))
            catch err0
                errs = try
                    @sync_end($var)
                    nothing
                catch errs
                    errs
                end
                rethrow(_merge_errors!(errs, err0))
            end
            @sync_end($var)
            ans
        end
        $(footer...)
    end
end

function _merge_errors!(errs, err0)
    if errs === nothing
        errs = CompositeException()
    elseif !(errs isa CompositeException)
        errs = CompositeException([errs])
    end
    pushfirst!(errs.exceptions, err0)
    return errs
end

"""
    Tapir.@spawn expression
"""
macro spawn(expr)
    var = esc(tokenname)
    quote
        @spawnin $var $(esc(expr))
    end
end

# precompile
const _NEVER = Ref(false)
function __init__()
    _NEVER[] || return
    tg = taskgroup()
    spawn!(tg, () -> nothing)
    sync!(tg)
end

end
