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
nodes inside the `lower_tapir_phic_output!` pass run just after slot2reg.  The PhiC
and Upsilon nodes that cross task boundaries are finally lowered to
`Tapir.UndefableRef` in `lower_tapir_task!`.  (Note: PhiC and Upsilon nodes
translate to stack memory in LLVM.  So, this representation may also be useful
for Tapir/LLVM integration.)

* TODO: Add an IR node that directly represents `_load`? It would reduce the
  transformation inside the macro.

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

"""
    shadow_vars(ex::Expr) -> ex′::Expr

If the syntax `\$x` is used in lhs/lvalue _or_ rhs/rvalue, replace it with a
local variable `x_shadow` that acts as a local copy of `\$x`. If `\$x` appears
as in lhs, also insert the "store" `\$x = x_shadow` (so that it can be
translated into set-many-load-once PhiC/Upsilon nodes).  If `\$x` appears in
`ex`, the variable `x` is declared to be local in `ex` to avoid introducing the
phi nodes in the continuation [^no_racy_phi].

That is to say, it transforms

    @spawn begin
        ...
        \$a = f(\$x)
    end

to

    let x_shadow
        if @isdefined x
            x_shadow = x
        end
        @raw_spawn begin
            local x  # required for avoiding "racy phi" [^no_racy_phi]
            # if @isdefined x_shadow
            #     x = x_shadow   # TODO: should we?
            # end
            ...
            x_shadow = f(x_shadow)
            \$x = x_shadow
        end
    end

[^no_racy_phi]: We need to make `x` a local variable when we have `\$x` because
    `x` may appear after the `end` of `@sync`. Since we "load" `\$x` only when
    it is defined (to support conditional task outputs; see `@sync` definition),
    a phi node has to be inserted after reattach for the variable live through
    the `else` branch of `@isdefined \$x`.
"""
function shadow_vars(ex::Expr)
    mapping = Dict{Symbol,Symbol}()  # x => x_shadow

    lookup(var::Symbol) =
        get!(mapping, var) do
            gensym(Symbol('_', '$', var, '_'))
        end

    function process(ex)
        @nospecialize
        ex isa Expr || return ex
        if (
            isassignment(ex) ||
            ex.head === :tuple && (i = findfirst(isassignment, ex.args)) !== nothing
        )
            lvars = Symbol[]
            if isassignment(ex)
                a1 = process_lhs!(lvars, ex.args[1])
                as = map(process, ex.args[2:end])
                ex = Expr(:(=), a1, as...)
            else
                # Handle: a, b, c = d, e, f
                args1 = [process_lhs!(lvars, a) for a in ex.args[1:i-1]]  # a, b
                arg2 = process_lhs!(lvars, ex.args[i].args[1])            # c
                args3 = map(process, ex.args[i].args[2:end])              # d
                args4 = map(process, ex.args[i+1:end])                    # e, f
                ex = Expr(:tuple, args1..., Expr(:(=), arg2, args3...), args4...)
            end
            if isempty(lvars)
                return ex
            else
                stores = map(lvars) do var
                    quote
                        $(Expr(:$, var)) = $(mapping[var])
                    end
                end
                return Expr(:block, ex, stores...)
            end
        elseif ex.head === :$
            if length(ex.args) == 1
                return lookup(ex.args[1])
            else
                return ex
            end
        elseif ex.head in (:macrocall, :meta, :inbounds, :loopinfo)
            return ex  # no recursion
        else
            return Expr(ex.head, map(process, ex.args)...)
        end
    end

    function process_lhs!(lvars::Vector{Symbol}, ex)
        @nospecialize
        ex isa Expr || return ex
        if ex.head === :$
            if length(ex.args) == 1
                var = ex.args[1]
                if var isa Symbol
                    shadow = lookup(var)
                    push!(lvars, var)
                    return shadow
                end
            end
        elseif ex.head === :tuple
            return Expr(ex.head, (process_lhs!(lvars, a) for a in ex.args)...)
        end
        return ex
    end

    ex = process(ex)

    vars_shadows = sort!(collect(mapping))

    header_locals = map(vars_shadows) do (var, shadow)
        quote
            local $shadow
            if $(Expr(:isdefined, var))
                $shadow = $var
            end
        end
    end
    header = Expr(:block, header_locals...)

    body = quote
        let $(map(first, vars_shadows)...)  # `let x1, x2, ...`
            $ex
        end
    end

    return header, body
end

function ensure_linenumbernode(__source__, @nospecialize(ex))
    if Meta.isexpr(ex, :block) && length(ex.args) > 0 && ex.args[1] isa LineNumberNode
        return ex
    else
        return Expr(:block, __source__, ex)
    end
end

macro spawnin(token, block)
    block = ensure_linenumbernode(__source__, block)
    header, body = shadow_vars(block)
    spawn = Expr(:spawn, esc(token), esc(body))
    return quote
        let
            $(esc(header))
            $spawn
        end
    end
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
        @nospecialize
        ex isa Expr || return ex
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

    function process(ex)
        @nospecialize
        ex isa Expr || return ex
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

Declare a scope in which Tapir tasks can be spawned using `Tapir.@spawn`.  The
tasks are synchronized upon exiting this block.

# Extended help

## Task output syntax `\$output`

Inside `Tapir.@sync` and `Tapir.@spawn`, the syntax `\$output` can be used for
writing (and reading, in `Tapir.@spawn`) variables outside `Tapir.@sync` block:

```julia
Tapir.@sync begin
    Tapir.@spawn \$x = f()
    \$y = g()
end
use(x, y)
```

If `\$x` is used inside a `Tapir.@spawn`, the variable `x` is automatically
marked as a local:

```julia
x = 0
y = 1
Tapir.@sync begin
    Tapir.@spawn begin

        x       # => throws UndefVarError
        x = \$x  # instead, we need to read the value explicitly

        v = y    # if we don't have `\$y`, we can read `y` in the outer scope

        \$x = \$x + v  # we can read and write \$x`
    end
    ...
end
use(x, y)
```

If `\$output` syntax is used, the user must make sure that there is only one
task writing to the `\$output` variable.  Note that `\$output` can appear
syntactically in multiple tasks.  That is to say, the following is valid as long
as `!(p && q)`:

```julia
Tapir.@sync begin
    Tapir.@spawn begin
        if p
            \$x = ...
        end
    end
    if q
        \$x = ...
    end
end
if p || q
    use(x)
end
```
"""
macro sync(block)
    var = esc(tokenname)
    block = macroexpand(__module__, block)  # for nested @sync
    block = Expr(:block, __source__, block)
    dict, block = output_vars(block)
    outputs = sort!(collect(dict))
    header = [:(local $(esc(slot))) for (_, slot) in outputs]
    footer = map(outputs) do (v, slot)
        quote
            if $(Expr(:isdefined, esc(slot)))
                $(esc(v)) = _load($(esc(slot)), $((QuoteNode(v))))
            else
                # We don't see `v` set in spawn since `v` would be declared to
                # be local by the `@spawn` macro [^no_racy_phi].
            end
        end
    end
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
    expr = ensure_linenumbernode(__source__, expr)
    esc(:($Tapir.@spawnin $tokenname $expr))
end

"""
    Tapir.@output v₍ v₂ … vₙ

Mark variables named `v₍`, `v₂`, ..., and `vₙ` as task outputs. This is
equivalent to `local v₍, v₂, …, vₙ` in terms of the scoping rule. However, they
must not be written or read in the code regions that may potentially be run in
parallel.
"""
macro output(exprs::Union{Symbol,Expr}...)
    variables = Symbol[]
    assignments = Expr[]
    for ex in exprs
        if ex isa Symbol
            push!(variables, ex)
        elseif Meta.isexpr(ex, :(=), 2) && ex.args[1] isa Symbol
            push!(variables, ex.args[1])
            push!(assignments, ex)
        end
    end

    locals = Expr(:local, variables...)
    outinfo = [Expr(:task_output, v, QuoteNode(v), __source__) for v in variables]

    esc(Expr(:block, __source__, locals, assignments..., outinfo...))
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
