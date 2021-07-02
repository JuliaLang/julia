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

#####
##### Julia-Tapir Frontend
#####

mutable struct Output{Name}
    x::Any
    Output{Name}() where {Name} = new{Name}()
end

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

output_vars!(ex) = output_vars!(Dict{Symbol,Symbol}(), ex)
output_vars!(outputs, _) = outputs
function output_vars!(outputs, ex::Expr)
    if isassignment(ex)
        lhs_output_vars!(outputs, ex, 1:1)
        for a in @view ex.args[2:end]
            output_vars!(outputs, a)
        end
    elseif Meta.isexpr(ex, :tuple) && (i = findfirst(isassignment, ex.args)) !== nothing
        # Handle: a, b, c = d, e, f
        lhs_output_vars!(outputs, ex, 1:i-1)  # a, b
        lhs_output_vars!(outputs, ex.args[i], 1:1)  # c
        for a in @view ex.args[i].args[2:end]
            output_vars!(outputs, a) # d
        end
        for a in @view ex.args[i+1:end]
            output_vars!(outputs, a) # e, f
        end
    else
        for a in ex.args
            output_vars!(outputs, a)
        end
    end
    return outputs
end

function lhs_output_vars!(outputs, ex, indices = eachindex(ex.args))
    for i in indices
        if Meta.isexpr(ex.args[i], :$, 1)
            v, = ex.args[i].args
            if v isa Symbol
                tmp = get!(() -> gensym("output_$(v)_"), outputs, v)
                ex.args[i] = :($tmp.x)
            end
        elseif Meta.isexpr(ex.args[i], :tuple)
            lhs_output_vars!(outputs, ex.args[i])
        end
    end
end

const tokenname = gensym(:token)

"""
    Tapir.@sync block
"""
macro sync(block)
    var = esc(tokenname)
    block = macroexpand(__module__, block)  # for nested @sync
    block = Expr(:block, __source__, block)
    dict = output_vars!(block)
    outputs = sort!(collect(dict))
    header = [:(local $(esc(tmp)) = Output{$(QuoteNode(v))}()) for (v, tmp) in outputs]
    footer = [:($(esc(v)) = $(esc(tmp)).x) for (v, tmp) in outputs]
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
