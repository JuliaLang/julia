# This file is a part of Julia. License is MIT: https://julialang.org/license

# Deprecated functions and objects
#
# Please add new deprecations at the bottom of the file.
# A function deprecated in a release will be removed in the next one.
# Please also add a reference to the pull request which introduced the
# deprecation.
#
# For simple cases where a direct replacement is available, use @deprecate:
# the first argument is the signature of the deprecated method, the second one
# is the call which replaces it. Remove the definition of the deprecated method
# and unexport it, as @deprecate takes care of calling the replacement
# and of exporting the function.
#
# For more complex cases, move the body of the deprecated method in this file,
# and call depwarn() directly from inside it. The symbol depwarn() expects is
# the name of the function, which is used to ensure that the deprecation warning
# is only printed the first time for each call place.

macro deprecate(old, new, ex=true)
    meta = Expr(:meta, :noinline)
    if isa(old, Symbol)
        oldname = Expr(:quote, old)
        newname = Expr(:quote, new)
        Expr(:toplevel,
            ex ? Expr(:export, esc(old)) : nothing,
            :(function $(esc(old))(args...)
                  $meta
                  depwarn($"`$old` is deprecated, use `$new` instead.", Core.Typeof($(esc(old))).name.mt.name)
                  $(esc(new))(args...)
              end))
    elseif isa(old, Expr) && (old.head == :call || old.head == :where)
        remove_linenums!(new)
        oldcall = sprint(show_unquoted, old)
        newcall = sprint(show_unquoted, new)
        # if old.head is a :where, step down one level to the :call to avoid code duplication below
        callexpr = old.head == :call ? old : old.args[1]
        if callexpr.head == :call
            if isa(callexpr.args[1], Symbol)
                oldsym = callexpr.args[1]::Symbol
            elseif isa(callexpr.args[1], Expr) && callexpr.args[1].head == :curly
                oldsym = callexpr.args[1].args[1]::Symbol
            else
                error("invalid usage of @deprecate")
            end
        else
            error("invalid usage of @deprecate")
        end
        Expr(:toplevel,
            ex ? Expr(:export, esc(oldsym)) : nothing,
            :($(esc(old)) = begin
                  $meta
                  depwarn($"`$oldcall` is deprecated, use `$newcall` instead.", Core.Typeof($(esc(oldsym))).name.mt.name)
                  $(esc(new))
              end))
    else
        error("invalid usage of @deprecate")
    end
end

function depwarn(msg, funcsym)
    opts = JLOptions()
    if opts.depwarn == 2
        throw(ErrorException(msg))
    end
    deplevel = opts.depwarn == 1 ? CoreLogging.Warn : CoreLogging.BelowMinLevel
    @logmsg(
        deplevel,
        msg,
        _module=begin
            bt = backtrace()
            frame, caller = firstcaller(bt, funcsym)
            # TODO: Is it reasonable to attribute callers without linfo to Core?
            caller.linfo isa Core.MethodInstance ? caller.linfo.def.module : Core
        end,
        _file=String(caller.file),
        _line=caller.line,
        _id=(frame,funcsym),
        _group=:depwarn,
        caller=caller,
        maxlog=funcsym === nothing ? nothing : 1
    )
    nothing
end

firstcaller(bt::Vector, ::Nothing) = Ptr{Cvoid}(0), StackTraces.UNKNOWN
firstcaller(bt::Vector, funcsym::Symbol) = firstcaller(bt, (funcsym,))
function firstcaller(bt::Vector, funcsyms)
    # Identify the calling line
    found = false
    lkup = StackTraces.UNKNOWN
    found_frame = Ptr{Cvoid}(0)
    for frame in bt
        lkups = StackTraces.lookup(frame)
        for outer lkup in lkups
            if lkup == StackTraces.UNKNOWN || lkup.from_c
                continue
            end
            if found
                found_frame = frame
                @goto found
            end
            found = lkup.func in funcsyms
            # look for constructor type name
            if !found && lkup.linfo isa Core.MethodInstance
                li = lkup.linfo
                ft = ccall(:jl_first_argument_datatype, Any, (Any,), li.def.sig)
                if isa(ft,DataType) && ft.name === Type.body.name
                    ft = unwrap_unionall(ft.parameters[1])
                    found = (isa(ft,DataType) && ft.name.name in funcsyms)
                end
            end
        end
    end
    return found_frame, StackTraces.UNKNOWN
    @label found
    return found_frame, lkup
end

deprecate(m::Module, s::Symbol, flag=1) = ccall(:jl_deprecate_binding, Cvoid, (Any, Any, Cint), m, s, flag)

macro deprecate_binding(old, new, export_old=true, dep_message=:nothing, constant=true)
    dep_message === :nothing && (dep_message = ", use $new instead.")
    return Expr(:toplevel,
         export_old ? Expr(:export, esc(old)) : nothing,
         Expr(:const, Expr(:(=), esc(Symbol(string("_dep_message_",old))), esc(dep_message))),
         constant ? Expr(:const, Expr(:(=), esc(old), esc(new))) : Expr(:(=), esc(old), esc(new)),
         Expr(:call, :deprecate, __module__, Expr(:quote, old)))
end

macro deprecate_stdlib(old, mod, export_old=true, newname=old)
    rename = old === newname ? "" : " as `$newname`"
    dep_message = """: it has been moved to the standard library package `$mod`$rename.
                        Add `using $mod` to your imports."""
    new = GlobalRef(Base.root_module(Base, mod), newname)
    return Expr(:toplevel,
         export_old ? Expr(:export, esc(old)) : nothing,
         Expr(:const, Expr(:(=), esc(Symbol(string("_dep_message_",old))), esc(dep_message))),
         Expr(:const, Expr(:(=), esc(old), esc(new))),
         Expr(:call, :deprecate, __module__, Expr(:quote, old)))
end

macro deprecate_moved(old, new, export_old=true)
    eold = esc(old)
    emsg = string(old, " has been moved to the package ", new, ".jl.\n",
        "Run `Pkg.add(\"", new, "\")` to install it, restart Julia,\n",
        "and then run `using ", new, "` to load it.")
    return Expr(:toplevel,
        :($eold(args...; kwargs...) = error($emsg)),
        export_old ? Expr(:export, eold) : nothing,
        Expr(:call, :deprecate, __module__, Expr(:quote, old), 2))
end


# BEGIN 0.6 deprecations

# removing the .op deprecations breaks a few things. TODO: fix
# deprecations for uses of old dot operators (.* etc) as objects, rather than
# just calling them infix.
for op in (:(!=), :≠, :+, :-, :*, :/, :÷, :%, :<, :(<=), :≤, :(==), :>, :>=, :≥, :\, :^, ://, :>>, :<<)
    dotop = Symbol('.', op)
    # define as const dotop = (a,b) -> ...
    # to work around syntax deprecation for dotop(a,b) = ...
    @eval const $dotop = (a,b) -> begin
        depwarn(string($(string(dotop)), " is no longer a function object, use `broadcast(",$op,", ...)` instead."),
                $(QuoteNode(dotop)))
        broadcast($op, a, b)
    end
    @eval export $dotop
end

# END 0.6 deprecations

# BEGIN 0.7 deprecations

# remove all support machinery in src for current_module

# PR #21956
# This mimics the structure as it was defined in Base to avoid directly breaking code
# that assumes this structure
module DFT
    for f in [:bfft, :bfft!, :brfft, :dct, :dct!, :fft, :fft!, :fftshift, :idct, :idct!,
              :ifft, :ifft!, :ifftshift, :irfft, :plan_bfft, :plan_bfft!, :plan_brfft,
              :plan_dct, :plan_dct!, :plan_fft, :plan_fft!, :plan_idct, :plan_idct!,
              :plan_ifft, :plan_ifft!, :plan_irfft, :plan_rfft, :rfft]
        pkg = endswith(String(f), "shift") ? "AbstractFFTs" : "FFTW"
        @eval Base.@deprecate_moved $f $pkg
    end
    module FFTW
        for f in [:r2r, :r2r!, :plan_r2r, :plan_r2r!]
            @eval Base.@deprecate_moved $f "FFTW"
        end
    end
    Base.deprecate(DFT, :FFTW, 2)
    export FFTW
end
using .DFT
for f in filter(s -> isexported(DFT, s), names(DFT, all = true))
    @eval export $f
end
module DSP
    for f in [:conv, :conv2, :deconv, :filt, :filt!, :xcorr]
        @eval Base.@deprecate_moved $f "DSP"
    end
end
deprecate(Base, :DSP, 2)
using .DSP
export conv, conv2, deconv, filt, filt!, xcorr

# PR #22325
# TODO: when this replace is removed from deprecated.jl:
# 1) rename the function replace_new from strings/util.jl to replace
# 2) update the replace(s::AbstractString, pat, f) method, below replace_new
#    (see instructions there)
function replace(s::AbstractString, pat, f, n::Integer)
    if n <= 0
        depwarn(string("`replace(s, pat, r, count)` with `count <= 0` is deprecated, use ",
                       "`replace(s, pat=>r, count=typemax(Int))` or `replace(s, pat=>r)` instead."),
                :replace)
        replace(s, pat=>f)
    else
        depwarn(string("`replace(s, pat, r, count)` is deprecated, use ",
                       "`replace(s, pat=>r, count=count)`"),
                :replace)
        replace(String(s), pat=>f, count=n)
    end
end

# PR #23066
@deprecate cfunction(f, r, a::Tuple) cfunction(f, r, Tuple{a...})
@noinline function cfunction(f, r, a)
    @nospecialize(f, r, a)
    depwarn("The function `cfunction` is now written as a macro `@cfunction`.", :cfunction)
    return ccall(:jl_function_ptr, Ptr{Cvoid}, (Any, Any, Any), f, r, a)
end
export cfunction

# PR #23271
# TODO: rename Base._IOContext to IOContext when this deprecation is deleted
function IOContext(io::IO; kws...)
    if isempty(kws) # Issue #25638
        _IOContext(io)
    else
        depwarn("`IOContext(io, k=v, ...)` is deprecated, use `IOContext(io, :k => v, ...)` instead.", :IOContext)
        IOContext(io, (k=>v for (k, v) in pairs(kws))...)
    end
end

@deprecate IOContext(io::IO, key, value) IOContext(io, key=>value)

# PR #23485
export countnz
function countnz(x)
    depwarn("`countnz(x)` is deprecated, use either `count(!iszero, x)` or `count(t -> t != 0, x)` instead.", :countnz)
    return count(t -> t != 0, x)
end

# PR #23690
# `SSHCredential` and `UserPasswordCredential` constructors using `prompt_if_incorrect`
# are deprecated in stdlib/LibGit2/types.jl.

# deprecate ones/zeros methods accepting an array as first argument
function ones(a::AbstractArray, ::Type{T}, dims::Tuple) where {T}
    depwarn(string("`ones(a::AbstractArray, ::Type{T}, dims::Tuple) where T` is ",
        "deprecated, use `fill!(similar(a, T, dims), 1)` instead, or ",
        "`fill!(similar(a, T, dims), one(T))` where necessary."), :ones)
    return fill!(similar(a, T, dims), one(T))
end
function ones(a::AbstractArray, ::Type{T}, dims...) where {T}
    depwarn(string("`ones(a::AbstractArray, ::Type{T}, dims...) where T` is ",
        "deprecated, use `fill!(similar(a, T, dims...), 1)` instead, or ",
        "`fill!(similar(a, T, dims...), one(T))` where necessary."), :ones)
    return fill!(similar(a, T, dims...), one(T))
end
function ones(a::AbstractArray, ::Type{T}) where {T}
    depwarn(string("`ones(a::AbstractArray, ::Type{T}) where T` is deprecated, ",
        "use `fill!(similar(a, T), 1)` instead, or `fill!(similar(a, T), one(T))` ",
        "where necessary."), :ones)
    return fill!(similar(a, T), one(T))
end
function ones(a::AbstractArray)
    depwarn(string("`ones(a::AbstractArray)` is deprecated, consider ",
        "`fill(1, size(a))`, `fill!(copy(a), 1)`, or `fill!(similar(a), 1)`. Where ",
        "necessary, use `fill!(similar(a), one(eltype(a)))`."), :ones)
    return fill!(similar(a), one(eltype(a)))
end

function zeros(a::AbstractArray, ::Type{T}, dims::Tuple) where {T}
    depwarn(string("`zeros(a::AbstractArray, ::Type{T}, dims::Tuple) where T` is ",
        "deprecated, use `fill!(similar(a, T, dims), 0)` instead, or ",
        "`fill!(similar(a, T, dims), zero(T))` where necessary."), :zeros)
    return fill!(similar(a, T, dims), zero(T))
end
function zeros(a::AbstractArray, ::Type{T}, dims...) where {T}
    depwarn(string("`zeros(a::AbstractArray, ::Type{T}, dims...) where T` is ",
        "deprecated, use `fill!(similar(a, T, dims...), 0)` instead, or ",
        "`fill!(similar(a, T, dims...), zero(T))` where necessary."), :zeros)
    return fill!(similar(a, T, dims...), zero(T))
end
function zeros(a::AbstractArray, ::Type{T}) where {T}
    depwarn(string("`zeros(a::AbstractArray, ::Type{T}) where T` is deprecated, ",
        "use `fill!(similar(a, T), 0)` instead, or `fill!(similar(a, T), zero(T))` ",
        "where necessary."), :zeros)
    return fill!(similar(a, T), zero(T))
end
function zeros(a::AbstractArray)
    depwarn(string("`zeros(a::AbstractArray)` is deprecated, consider `zero(a)`, ",
        "`fill(0, size(a))`, `fill!(copy(a), 0)`, or ",
        "`fill!(similar(a), 0)`. Where necessary, use ",
        "`fill!(similar(a), zero(eltype(a)))`."), :zeros)
    return fill!(similar(a), zero(eltype(a)))
end

# A[I...] .= with scalar indices should modify the element at A[I...]
function Broadcast.dotview(A::AbstractArray, args::Number...)
    depwarn("the behavior of `A[I...] .= X` with scalar indices will change in the future. Use `A[I...] = X` instead.", :broadcast!)
    view(A, args...)
end
Broadcast.dotview(A::AbstractArray{<:AbstractArray}, args::Integer...) = getindex(A, args...)
# Upon removing deprecations, also enable the @testset "scalar .=" in test/broadcast.jl

# indexing with A[true] will throw an argument error in the future
function to_index(i::Bool)
    depwarn("indexing with Bool values is deprecated. Convert the index to an integer first with `Int(i)`.", (:getindex, :setindex!, :view))
    convert(Int,i)::Int
end
# After deprecation is removed, enable the @testset "indexing by Bool values" in test/arrayops.jl
# Also un-comment the new definition in base/indices.jl

# Broadcast no longer defaults to treating its arguments as scalar (#)
@noinline function Broadcast.broadcastable(x)
    depwarn("""
        broadcast will default to iterating over its arguments in the future. Wrap arguments of
        type `x::$(typeof(x))` with `Ref(x)` to ensure they broadcast as "scalar" elements.
        """, (:broadcast, :broadcast!))
    return Ref{typeof(x)}(x)
end
@eval Base.Broadcast Base.@deprecate_binding Scalar DefaultArrayStyle{0} false
# After deprecation is removed, enable the fallback broadcastable definitions in base/broadcast.jl

# deprecate BitArray{...}(shape...) constructors to BitArray{...}(undef, shape...) equivalents
@deprecate BitArray{N}(dims::Vararg{Int,N}) where {N}   BitArray{N}(undef, dims)
@deprecate BitArray(dims::NTuple{N,Int}) where {N}      BitArray(undef, dims...)
@deprecate BitArray(dims::Integer...)                   BitArray(undef, dims)

## deprecate full
export full
# full no-op fallback
function full(A::AbstractArray)
    depwarn(string(
        "The no-op `full(A::AbstractArray)` fallback has been deprecated, and no more ",
        "specific `full` method for $(typeof(A)) exists. Furthermore, `full` in general ",
        "has been deprecated.\n\n",
        "To replace `full(A)`, as appropriate consider disambiguating with a concrete ",
        "array constructor (e.g. `Array(A)`), with an abstract array constructor (e.g.`AbstractArray(A)`), ",
        "instead `convert`ing to an array type (e.g `convert(Array, A)`, `convert(AbstractArray, A)`), ",
        "or using another such operation that addresses your specific use case."),  :full)
    return A
end

# Broadcast extension API (#23939)
@eval Broadcast begin
    Base.@deprecate_binding containertype combine_styles false
    Base.@deprecate_binding _containertype BroadcastStyle false
    Base.@deprecate_binding promote_containertype BroadcastStyle false
    Base.@deprecate_binding broadcast_c! broadcast! false ", `broadcast_c!(f, ::Type, ::Type, C, As...)` should become `broadcast!(f, C, As...)` (see the manual chapter Interfaces)"
    Base.@deprecate_binding broadcast_c broadcast false ", `broadcast_c(f, ::Type{C}, As...)` should become `broadcast(f, C, nothing, nothing, As...))` (see the manual chapter Interfaces)"
    Base.@deprecate_binding broadcast_t broadcast false ", `broadcast_t(f, ::Type{ElType}, shape, iter, As...)` should become `broadcast(f, Broadcast.DefaultArrayStyle{N}(), ElType, shape, As...))` (see the manual chapter Interfaces)"
end

# 24490 - warnings and messages
const log_info_to = Dict{Tuple{Union{Module,Nothing},Union{Symbol,Nothing}},IO}()
const log_warn_to = Dict{Tuple{Union{Module,Nothing},Union{Symbol,Nothing}},IO}()
const log_error_to = Dict{Tuple{Union{Module,Nothing},Union{Symbol,Nothing}},IO}()

function _redirect(io::IO, log_to::Dict, sf::StackTraces.StackFrame)
    (sf.linfo isa Core.MethodInstance) || return io
    mod = sf.linfo.def
    isa(mod, Method) && (mod = mod.module)
    fun = sf.func
    if haskey(log_to, (mod,fun))
        return log_to[(mod,fun)]
    elseif haskey(log_to, (mod,nothing))
        return log_to[(mod,nothing)]
    elseif haskey(log_to, (nothing,nothing))
        return log_to[(nothing,nothing)]
    else
        return io
    end
end

function _redirect(io::IO, log_to::Dict, fun::Symbol)
    clos = string("#",fun,"#")
    kw = string("kw##",fun)
    local sf
    break_next_frame = false
    for trace in backtrace()
        stack::Vector{StackFrame} = StackTraces.lookup(trace)
        filter!(frame -> !frame.from_c, stack)
        for frame in stack
            (frame.linfo isa Core.MethodInstance) || continue
            sf = frame
            break_next_frame && (@goto skip)
            mod = frame.linfo.def
            isa(mod, Method) && (mod = mod.module)
            mod === Base || continue
            sff = string(frame.func)
            if frame.func == fun || startswith(sff, clos) || startswith(sff, kw)
                break_next_frame = true
            end
        end
    end
    @label skip
    _redirect(io, log_to, sf)
end

@inline function redirect(io::IO, log_to::Dict, arg::Union{Symbol,StackTraces.StackFrame})
    if isempty(log_to)
        return io
    else
        if length(log_to)==1 && haskey(log_to,(nothing,nothing))
            return log_to[(nothing,nothing)]
        else
            return _redirect(io, log_to, arg)
        end
    end
end

"""
    logging(io [, m [, f]][; kind=:all])
    logging([; kind=:all])

Stream output of informational, warning, and/or error messages to `io`,
overriding what was otherwise specified.  Optionally, divert stream only for
module `m`, or specifically function `f` within `m`.  `kind` can be `:all` (the
default), `:info`, `:warn`, or `:error`.  See `Base.log_{info,warn,error}_to`
for the current set of redirections.  Call `logging` with no arguments (or just
the `kind`) to reset everything.
"""
function logging(io::IO, m::Union{Module,Nothing}=nothing, f::Union{Symbol,Nothing}=nothing;
                 kind::Symbol=:all)
    depwarn("""`logging()` is deprecated, use `with_logger` instead to capture
               messages from `Base`.""", :logging)
    (kind==:all || kind==:info)  && (log_info_to[(m,f)] = io)
    (kind==:all || kind==:warn)  && (log_warn_to[(m,f)] = io)
    (kind==:all || kind==:error) && (log_error_to[(m,f)] = io)
    nothing
end

function logging(;  kind::Symbol=:all)
    depwarn("""`logging()` is deprecated, use `with_logger` instead to capture
               messages from `Base`.""", :logging)
    (kind==:all || kind==:info)  && empty!(log_info_to)
    (kind==:all || kind==:warn)  && empty!(log_warn_to)
    (kind==:all || kind==:error) && empty!(log_error_to)
    nothing
end

"""
    info([io, ] msg..., [prefix="INFO: "])

Display an informational message.
Argument `msg` is a string describing the information to be displayed.
The `prefix` keyword argument can be used to override the default
prepending of `msg`.

# Examples
```jldoctest
julia> info("hello world")
INFO: hello world

julia> info("hello world"; prefix="MY INFO: ")
MY INFO: hello world
```

See also [`logging`](@ref).
"""
function info(io::IO, msg...; prefix="INFO: ")
    depwarn("`info()` is deprecated, use `@info` instead.", :info)
    buf = IOBuffer()
    iob = redirect(IOContext(buf, io), log_info_to, :info)
    printstyled(iob, prefix; bold=true, color=info_color())
    printstyled(iob, chomp(string(msg...)), '\n', color=info_color())
    print(io, String(take!(buf)))
    return
end
info(msg...; prefix="INFO: ") = info(stderr, msg..., prefix=prefix)

# print a warning only once

const have_warned = Set()

warn_once(io::IO, msg...) = warn(io, msg..., once=true)
warn_once(msg...) = warn(stderr, msg..., once=true)

"""
    warn([io, ] msg..., [prefix="WARNING: ", once=false, key=nothing, bt=nothing, filename=nothing, lineno::Int=0])

Display a warning. Argument `msg` is a string describing the warning to be
displayed.  Set `once` to true and specify a `key` to only display `msg` the
first time `warn` is called.  If `bt` is not `nothing` a backtrace is displayed.
If `filename` is not `nothing` both it and `lineno` are displayed.

See also [`logging`](@ref).
"""
function warn(io::IO, msg...;
              prefix="WARNING: ", once=false, key=nothing, bt=nothing,
              filename=nothing, lineno::Int=0)
    depwarn("`warn()` is deprecated, use `@warn` instead.", :warn)
    str = chomp(string(msg...))
    if once
        if key === nothing
            key = str
        end
        (key in have_warned) && return
        push!(have_warned, key)
    end
    buf = IOBuffer()
    iob = redirect(IOContext(buf, io), log_warn_to, :warn)
    printstyled(iob, prefix; bold=true, color=warn_color())
    printstyled(iob, str, color=warn_color())
    if bt !== nothing
        show_backtrace(iob, bt)
    end
    if filename !== nothing
        print(iob, "\nin expression starting at $filename:$lineno")
    end
    println(iob)
    print(io, String(take!(buf)))
    return
end

"""
    warn(msg)

Display a warning. Argument `msg` is a string describing the warning to be displayed.

# Examples
```jldoctest
julia> warn("Beep Beep")
WARNING: Beep Beep
```
"""
warn(msg...; kw...) = warn(stderr, msg...; kw...)

warn(io::IO, err::Exception; prefix="ERROR: ", kw...) =
    warn(io, sprint(showerror, err), prefix=prefix; kw...)

warn(err::Exception; prefix="ERROR: ", kw...) =
    warn(stderr, err, prefix=prefix; kw...)

info(io::IO, err::Exception; prefix="ERROR: ", kw...) =
    info(io, sprint(showerror, err), prefix=prefix; kw...)

info(err::Exception; prefix="ERROR: ", kw...) =
    info(stderr, err, prefix=prefix; kw...)

# PR #25057
@deprecate indices(a) axes(a)
@deprecate indices(a, d) axes(a, d)

# And similar _indices names in Broadcast
@eval Broadcast Base.@deprecate_binding broadcast_indices broadcast_axes true
@eval Broadcast Base.@deprecate_binding check_broadcast_indices check_broadcast_axes false

# PR #25046
export reload, workspace
reload(name::AbstractString) = error("`reload($(repr(name)))` is discontinued, consider Revise.jl for an alternative workflow.")
workspace() = error("`workspace()` is discontinued, consider Revise.jl for an alternative workflow.")

# Issue #12902
@deprecate parentindexes parentindices

# sub2ind and ind2sub deprecation (PR #24715)
_ind2sub_depwarn(x, y) = "`ind2sub($x, $y)` is deprecated, use `Tuple(CartesianIndices($x)[$y])` for a direct replacement. In many cases, the conversion to `Tuple` is not necessary."
function ind2sub(A::AbstractArray, ind)
    depwarn(_ind2sub_depwarn("A", "ind"), :ind2sub)
    Tuple(CartesianIndices(A)[ind])
end
function ind2sub(::Tuple{}, ind::Integer)
    depwarn(_ind2sub_depwarn("()", "ind"), :ind2sub)
    Tuple(CartesianIndices(())[ind])
end
function ind2sub(dims::Tuple{Vararg{Integer,N}} where N, ind::Integer)
    depwarn(_ind2sub_depwarn("dims", "ind"), :ind2sub)
    Tuple(CartesianIndices(dims)[ind])
end
function ind2sub(inds::Tuple{Base.OneTo}, ind::Integer)
    depwarn(_ind2sub_depwarn("inds", "ind"), :ind2sub)
    Tuple(CartesianIndices(inds)[ind])
end
function ind2sub(inds::Tuple{AbstractUnitRange}, ind::Integer)
    depwarn(_ind2sub_depwarn("inds", "ind"), :ind2sub)
    Tuple(CartesianIndices(inds)[ind])
end
function ind2sub(inds::Tuple{Vararg{AbstractUnitRange,N}} where N, ind::Integer)
    depwarn(_ind2sub_depwarn("inds", "ind"), :ind2sub)
    Tuple(CartesianIndices(inds)[ind])
end
function ind2sub(inds::Union{DimsInteger{N},Indices{N}}  where N, ind::AbstractVector{<:Integer})
    depwarn(_ind2sub_depwarn("inds", "ind"), :ind2sub)
    Tuple(CartesianIndices(inds)[ind])
end

@deprecate sub2ind(A::AbstractArray, I...) LinearIndices(A)[I...]
@deprecate sub2ind(dims::Tuple{}) LinearIndices(dims)[]
@deprecate sub2ind(dims::DimsInteger) LinearIndices(dims)[]
@deprecate sub2ind(dims::Indices) LinearIndices(dims)[]
@deprecate sub2ind(dims::Tuple{}, I::Integer...) LinearIndices(dims)[I...]
@deprecate sub2ind(dims::DimsInteger, I::Integer...) LinearIndices(dims)[I...]
@deprecate sub2ind(inds::Indices, I::Integer...) LinearIndices(inds)[I...]
@deprecate sub2ind(inds::Tuple{OneTo}, I::Integer...) LinearIndices(inds)[I...]
@deprecate sub2ind(inds::Tuple{OneTo}, i::Integer) LinearIndices(inds)[i]
@deprecate sub2ind(inds::Tuple{OneTo}, I1::AbstractVector{T}, I::AbstractVector{T}...) where {T<:Integer} LinearIndices(inds)[CartesianIndex.(I1, I...)]
@deprecate sub2ind(inds::Union{DimsInteger,Indices}, I1::AbstractVector{T}, I::AbstractVector{T}...) where {T<:Integer} LinearIndices(inds)[CartesianIndex.(I1, I...)]

# PR #25113
@deprecate_binding CartesianRange CartesianIndices

# PR 21527
@deprecate Ref(x::AbstractArray) Ref(x, 1)
@deprecate Ref(x::Ptr) Ref(x, 1)
@deprecate Ref(x::Ref) x # or perhaps, `convert(Ref, x)`

# Issues #17812 Remove default stride implementation
function strides(a::AbstractArray)
    depwarn("""
    The default `strides(a::AbstractArray)` implementation is deprecated for general arrays.
    Specialize `strides(::$(typeof(a).name))` if `$(typeof(a).name)` indeed uses a strided representation in memory.
    Warning: inappropriately implementing this method for an array type that does not use strided
    storage may lead to incorrect results or segfaults.
    """, :strides)
    size_to_strides(1, size(a)...)
end

@deprecate substrides(s, parent, dim, I::Tuple) substrides(parent, strides(parent), I)

# Issue #26072 Also remove default Base.elsize implementation
function elsize(t::Type{<:AbstractArray{T}}) where T
    depwarn("""
    The default `Base.elsize(::Type{<:AbstractArray})` implementation is deprecated for general arrays.
    Specialize `Base.elsize(::Type{<:$(t.name)})` if `$(t.name)` indeed has a known representation
    in memory such that it represents the distance between two contiguous elements.
    Warning: inappropriately implementing this method may lead to incorrect results or segfaults.
    """, :elsize)
    sizeof(T)
end

# PR #26284
@deprecate (+)(i::Integer, index::CartesianIndex) (i*one(index) + index)
@deprecate (+)(index::CartesianIndex, i::Integer) (index + i*one(index))
@deprecate (-)(i::Integer, index::CartesianIndex) (i*one(index) - index)
@deprecate (-)(index::CartesianIndex, i::Integer) (index - i*one(index))

# PR #26347: Deprecate implicit scalar broadcasting in setindex!
_axes(::Ref) = ()
_axes(x) = axes(x)
setindex_shape_check(X::Base.Iterators.Repeated, I...) = nothing
function deprecate_scalar_setindex_broadcast_message(v, I...)
    value = (_axes(Base.Broadcast.broadcastable(v)) == () ? "x" : "(x,)")
    "using `A[I...] = x` to implicitly broadcast `x` across many locations is deprecated. Use `A[I...] .= $value` instead."
end
deprecate_scalar_setindex_broadcast_message(v, ::Colon, ::Vararg{Colon}) =
    "using `A[:] = x` to implicitly broadcast `x` across many locations is deprecated. Use `fill!(A, x)` instead."

function _iterable(v, I...)
    depwarn(deprecate_scalar_setindex_broadcast_message(v, I...), :setindex!)
    Iterators.repeated(v)
end
function setindex!(B::BitArray, x, I0::Union{Colon,UnitRange{Int}}, I::Union{Int,UnitRange{Int},Colon}...)
    depwarn(deprecate_scalar_setindex_broadcast_message(x, I0, I...), :setindex!)
    B[I0, I...] .= (x,)
    B
end


# Remove ambiguous CartesianIndices and LinearIndices constructors that are ambiguous between an axis and an array (#26448)
@eval IteratorsMD begin
    import Base: LinearIndices
    @deprecate CartesianIndices(inds::Vararg{AbstractUnitRange{Int},N}) where {N} CartesianIndices(inds)
    @deprecate CartesianIndices(inds::Vararg{AbstractUnitRange{<:Integer},N}) where {N} CartesianIndices(inds)
    @deprecate LinearIndices(inds::Vararg{AbstractUnitRange{Int},N}) where {N} LinearIndices(inds)
    @deprecate LinearIndices(inds::Vararg{AbstractUnitRange{<:Integer},N}) where {N} LinearIndices(inds)
    # preserve the case with N = 1 (only needed as long as the above deprecations are here)
    CartesianIndices(inds::AbstractUnitRange{Int}) = CartesianIndices(axes(inds))
    CartesianIndices(inds::AbstractUnitRange{<:Integer}) = CartesianIndices(axes(inds))
    LinearIndices(inds::AbstractUnitRange{Int}) = LinearIndices(axes(inds))
    LinearIndices(inds::AbstractUnitRange{<:Integer}) = LinearIndices(axes(inds))
end

# END 0.7 deprecations

# BEGIN 1.0 deprecations

# END 1.0 deprecations
