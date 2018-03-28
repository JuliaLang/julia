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
    @gensym oldmtname
    if isa(old, Symbol)
        oldname = Expr(:quote, old)
        newname = Expr(:quote, new)
        Expr(:toplevel,
            ex ? Expr(:export, esc(old)) : nothing,
            :(function $(esc(old))(args...)
                  $meta
                  depwarn($"`$old` is deprecated, use `$new` instead.", $oldmtname)
                  $(esc(new))(args...)
              end),
            :(const $oldmtname = Core.Typeof($(esc(old))).name.mt.name))
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
                  depwarn($"`$oldcall` is deprecated, use `$newcall` instead.", $oldmtname)
                  $(esc(new))
              end),
            :(const $oldmtname = Core.Typeof($(esc(oldsym))).name.mt.name))
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
        maxlog=1
    )
    nothing
end

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

# Deprecate promote_eltype_op (#19814, #19937)
_promote_eltype_op(::Any) = Any
_promote_eltype_op(op, A) = (@_inline_meta; promote_op(op, eltype(A)))
_promote_eltype_op(op, A, B) = (@_inline_meta; promote_op(op, eltype(A), eltype(B)))
_promote_eltype_op(op, A, B, C, D...) = (@_inline_meta; _promote_eltype_op(op, eltype(A), _promote_eltype_op(op, B, C, D...)))
@inline function promote_eltype_op(args...)
    depwarn("""
            `promote_eltype_op` is deprecated and should not be used.
            See https://github.com/JuliaLang/julia/issues/19669.""",
            :promote_eltype_op)
    _promote_eltype_op(args...)
end

# END 0.6 deprecations

# BEGIN 0.7 deprecations

# TODO: remove warning for using `_` in parse_input_line in base/client.jl

@deprecate issubtype (<:)

@deprecate union() Set()

# 12807
start(::Union{Process, ProcessChain}) = 1
done(::Union{Process, ProcessChain}, i::Int) = (i == 3)
next(p::Union{Process, ProcessChain}, i::Int) = (getindex(p, i), i + 1)
@noinline function getindex(p::Union{Process, ProcessChain}, i::Int)
    depwarn("`open(cmd)` now returns only a Process<:IO object.", :getindex)
    return i == 1 ? getfield(p, p.openstream) : p
end

# also remove all support machinery in src for current_module when removing this deprecation
# and make Base.include an error
_current_module() = ccall(:jl_get_current_module, Ref{Module}, ())
@noinline function binding_module(s::Symbol)
    depwarn("`binding_module(symbol)` is deprecated, use `binding_module(module, symbol)` instead.", :binding_module)
    return binding_module(_current_module(), s)
end
export expand
@noinline function expand(@nospecialize(x))
    depwarn("`expand(x)` is deprecated, use `Meta.lower(module, x)` instead.", :expand)
    return Meta.lower(_current_module(), x)
end
@noinline function macroexpand(@nospecialize(x))
    depwarn("`macroexpand(x)` is deprecated, use `macroexpand(module, x)` instead.", :macroexpand)
    return macroexpand(_current_module(), x)
end
@noinline function isconst(s::Symbol)
    depwarn("`isconst(symbol)` is deprecated, use `isconst(module, symbol)` instead.", :isconst)
    return isconst(_current_module(), s)
end
@noinline function include_string(txt::AbstractString, fname::AbstractString)
    depwarn("`include_string(string, fname)` is deprecated, use `include_string(module, string, fname)` instead.", :include_string)
    return include_string(_current_module(), txt, fname)
end
@noinline function include_string(txt::AbstractString)
    depwarn("`include_string(string)` is deprecated, use `include_string(module, string)` instead.", :include_string)
    return include_string(_current_module(), txt, "string")
end

"""
    current_module() -> Module

Get the *dynamically* current `Module`, which is the `Module` code is currently being read
from. In general, this is not the same as the module containing the call to this function.

DEPRECATED: use @__MODULE__ instead
"""
@noinline function current_module()
    depwarn("`current_module()` is deprecated, use `@__MODULE__` instead.", :current_module)
    return _current_module()
end
export current_module

@deprecate_binding colon (:)

module Operators
    for op in [:!, :(!=), :(!==), :%, :&, :*, :+, :-, :/, ://, :<, :<:, :<<, :(<=),
               :<|, :(==), :(===), :>, :>:, :(>=), :>>, :>>>, :\, :^,
               :adjoint, :getindex, :hcat, :hvcat, :setindex!, :transpose, :vcat,
               :xor, :|, :|>, :~, :×, :÷, :∈, :∉, :∋, :∌, :∘, :√, :∛, :∩, :∪, :≠, :≤,
               :≥, :⊆, :⊈, :⊊, :⊻, :⋅]
        if isdefined(Base, op)
            @eval Base.@deprecate_binding $op Base.$op
        end
    end
    Base.@deprecate_binding colon (:)
end
export Operators

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

# PR #21709
@deprecate cov(x::AbstractVector, corrected::Bool) cov(x, corrected=corrected)
@deprecate cov(x::AbstractMatrix, vardim::Int, corrected::Bool) cov(x, dims=vardim, corrected=corrected)
@deprecate cov(X::AbstractVector, Y::AbstractVector, corrected::Bool) cov(X, Y, corrected=corrected)
@deprecate cov(X::AbstractVecOrMat, Y::AbstractVecOrMat, vardim::Int, corrected::Bool) cov(X, Y, dims=vardim, corrected=corrected)

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

@deprecate replace(s::AbstractString, pat, f) replace(s, pat=>f)

# PR #22475
@deprecate ntuple(f, ::Type{Val{N}}) where {N}  ntuple(f, Val(N))
@deprecate fill_to_length(t, val, ::Type{Val{N}}) where {N} fill_to_length(t, val, Val(N)) false
@deprecate literal_pow(a, b, ::Type{Val{N}}) where {N} literal_pow(a, b, Val(N)) false
@eval IteratorsMD @deprecate split(t, V::Type{Val{n}}) where {n} split(t, Val(n)) false
@deprecate cat(::Type{Val{N}}, A::AbstractArray...) where {N} cat(Val(N), A...)
@deprecate cat_t(::Type{Val{N}}, ::Type{T}, A, B) where {N,T} cat_t(Val(N), T, A, B) false
@deprecate reshape(A::AbstractArray, ::Type{Val{N}}) where {N} reshape(A, Val(N))

@deprecate read(s::IO, x::Ref) read!(s, x)

@deprecate read(s::IO, t::Type, d1::Int, dims::Int...) read!(s, Array{t}(undef, d1, dims...))
@deprecate read(s::IO, t::Type, d1::Integer, dims::Integer...) read!(s, Array{t}(undef, d1, dims...))
@deprecate read(s::IO, t::Type, dims::Dims) read!(s, Array{t}(undef, dims))

function CartesianIndices(start::CartesianIndex{N}, stop::CartesianIndex{N}) where N
    inds = map((f,l)->f:l, start.I, stop.I)
    depwarn("the internal representation of CartesianIndices has changed, use `CartesianIndices($inds)` (or other more appropriate AbstractUnitRange type) instead.", :CartesianIndices)
    CartesianIndices(inds)
end

# PR #20005
function InexactError()
    depwarn("InexactError now supports arguments, use `InexactError(funcname::Symbol, ::Type, value)` instead.", :InexactError)
    InexactError(:none, Any, nothing)
end

# PR #22751
function DomainError()
    depwarn("DomainError now supports arguments, use `DomainError(value)` or `DomainError(value, msg)` instead.", :DomainError)
    DomainError(nothing)
end

# PR #22761
function OverflowError()
    depwarn("OverflowError now supports a message string, use `OverflowError(msg)` instead.", :OverflowError)
    OverflowError("")
end

@deprecate fieldnames(v) fieldnames(typeof(v))
# nfields(::Type) deprecation in builtins.c: update nfields tfunc in compiler/tfuncs.jl when it is removed.
# also replace `_nfields` with `nfields` in summarysize.c when this is removed.

# ::ANY is deprecated in src/method.c
# also remove all instances of `jl_ANY_flag` in src/

# issue #13079
# in julia-parser.scm:
#     move prec-bitshift after prec-rational
#     remove parse-with-chains-warn and bitshift-warn
# update precedence table in doc/src/manual/mathematical-operations.md

# PR #22182
@deprecate is_apple   Sys.isapple
@deprecate is_bsd     Sys.isbsd
@deprecate is_linux   Sys.islinux
@deprecate is_unix    Sys.isunix
@deprecate is_windows Sys.iswindows

@deprecate read(cmd::AbstractCmd, stdin::Redirectable) read(pipeline(stdin, cmd))
@deprecate readstring(cmd::AbstractCmd, stdin::Redirectable) readstring(pipeline(stdin, cmd))
@deprecate eachline(cmd::AbstractCmd, stdin; kw...) eachline(pipeline(stdin, cmd), kw...)

@deprecate showall(x)     show(x)
@deprecate showall(io, x) show(IOContext(io, :limit => false), x)

@deprecate_binding AbstractIOBuffer GenericIOBuffer false

@deprecate String(io::GenericIOBuffer) String(take!(copy(io)))

@deprecate readstring(s::IO) read(s, String)
@deprecate readstring(filename::AbstractString) read(filename, String)
@deprecate readstring(cmd::AbstractCmd) read(cmd, String)

# issue #11310
# remove "parametric method syntax" deprecation in julia-syntax.scm

@deprecate momenttype(::Type{T}) where {T} typeof((zero(T)*zero(T) + zero(T)*zero(T))/2) false

# issue #6466
# `write` on non-isbits arrays is deprecated in io.jl.

# PR #23187
@deprecate cpad(s, n::Integer, p=" ") rpad(lpad(s, div(n+textwidth(s), 2), p), n, p) false

# PR #22088
function hex2num(s::AbstractString)
    depwarn("`hex2num(s)` is deprecated. Use `reinterpret(Float64, parse(UInt64, s, base = 16))` instead.", :hex2num)
    if length(s) <= 4
        return reinterpret(Float16, parse(UInt16, s, base = 16))
    end
    if length(s) <= 8
        return reinterpret(Float32, parse(UInt32, s, base = 16))
    end
    return reinterpret(Float64, parse(UInt64, s, base = 16))
end
export hex2num

@deprecate num2hex(x::Union{Float16,Float32,Float64}) string(reinterpret(Unsigned, x), base = 16, pad = sizeof(x)*2)
@deprecate num2hex(n::Integer) string(n, base = 16, pad = sizeof(n)*2)

# PR #22742: change in isapprox semantics
@deprecate rtoldefault(x,y) rtoldefault(x,y,0) false

# PR #23235
@deprecate ctranspose adjoint
@deprecate ctranspose! adjoint!

function convert(::Union{Type{Vector{UInt8}}, Type{Array{UInt8}}}, s::AbstractString)
    depwarn("Strings can no longer be `convert`ed to byte arrays. Use `unsafe_wrap` or `codeunits` instead.", :Type)
    unsafe_wrap(Vector{UInt8}, String(s))
end
function (::Type{Vector{UInt8}})(s::String)
    depwarn("Vector{UInt8}(s::String) will copy data in the future. To avoid copying, use `unsafe_wrap` or `codeunits` instead.", :Type)
    unsafe_wrap(Vector{UInt8}, s)
end
function (::Type{Array{UInt8}})(s::String)
    depwarn("Array{UInt8}(s::String) will copy data in the future. To avoid copying, use `unsafe_wrap` or `codeunits` instead.", :Type)
    unsafe_wrap(Vector{UInt8}, s)
end

@deprecate convert(::Type{Vector{Char}}, s::AbstractString)   Vector{Char}(s)
@deprecate convert(::Type{Symbol}, s::AbstractString)         Symbol(s)
@deprecate convert(::Type{String}, s::Symbol)                 String(s)
@deprecate convert(::Type{String}, v::Vector{UInt8})          String(v)
@deprecate convert(::Type{S}, g::Unicode.GraphemeIterator) where {S<:AbstractString}  convert(S, g.s)
@deprecate convert(::Type{String}, v::AbstractVector{Char})   String(v)

@deprecate convert(::Type{Libc.FILE}, s::IO)  Libc.FILE(s)
@deprecate convert(::Type{VersionNumber}, v::Integer)         VersionNumber(v)
@deprecate convert(::Type{VersionNumber}, v::Tuple)           VersionNumber(v)
@deprecate convert(::Type{VersionNumber}, v::AbstractString)  VersionNumber(v)

@deprecate (convert(::Type{Integer}, x::Enum{T}) where {T<:Integer})         Integer(x)
@deprecate (convert(::Type{T}, x::Enum{T2}) where {T<:Integer,T2<:Integer})  T(x)

function (::Type{T})(arg) where {T}
    if applicable(convert, T, arg)
        sig = which(convert, (Type{T}, typeof(arg))).sig
        if sig == (Tuple{typeof(convert),Type{S},Number} where S<:Number) ||
           sig == (Tuple{typeof(convert),Type{S},AbstractArray} where S<:AbstractArray)
            # matches a catch-all converter; will stack overflow
            throw(MethodError(T, (arg,)))
        end
        # if `convert` call would not work, just let the method error happen
        depwarn("Constructors no longer fall back to `convert`. A constructor `$T(::$(typeof(arg)))` should be defined instead.", :Type)
    end
    convert(T, arg)::T
end
# related items to remove in: abstractarray.jl, dates/periods.jl, compiler.jl
# also remove all uses of is_default_method

@deprecate convert(::Type{UInt128},     u::UUID)     UInt128(u)
@deprecate convert(::Type{UUID}, s::AbstractString)  UUID(s)

# Issue #19923
@deprecate ror                  circshift
@deprecate ror!                 circshift!
@deprecate rol(B, i)            circshift(B, -i)
@deprecate rol!(dest, src, i)   circshift!(dest, src, -i)
@deprecate rol!(B, i)           circshift!(B, -i)

# issue #5148, PR #23259
# warning for `const` on locals should be changed to an error in julia-syntax.scm

# issue #22789
# remove code for `importall` in src/

# issue #17886
# deprecations for filter[!] with 2-arg functions are in abstractdict.jl

# PR #23066
@deprecate cfunction(f, r, a::Tuple) cfunction(f, r, Tuple{a...})

# PR 23341
@eval GMP @deprecate gmp_version() version() false
@eval GMP @Base.deprecate_binding GMP_VERSION VERSION false
@eval GMP @deprecate gmp_bits_per_limb() bits_per_limb() false
@eval GMP @Base.deprecate_binding GMP_BITS_PER_LIMB BITS_PER_LIMB false
@eval MPFR @deprecate get_version() version() false

# PR #23427
@deprecate_binding e          ℯ true ", use ℯ (\\euler) or `Base.MathConstants.e`"
@deprecate_binding eu         ℯ true ", use ℯ (\\euler) or `Base.MathConstants.e`"
@deprecate_binding γ          MathConstants.γ
@deprecate_binding eulergamma MathConstants.eulergamma
@deprecate_binding catalan    MathConstants.catalan
@deprecate_binding φ          MathConstants.φ
@deprecate_binding golden     MathConstants.golden

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

# issue #14470
# TODO: More deprecations must be removed in src/cgutils.cpp:emit_array_nd_index()
# TODO: Re-enable the disabled tests marked PLI
# On the Julia side, this definition will gracefully supercede the new behavior (already coded)
@inline function checkbounds_indices(::Type{Bool}, IA::Tuple{Any,Vararg{Any}}, ::Tuple{})
    any(x->unsafe_length(x)==0, IA) && return false
    any(x->unsafe_length(x)!=1, IA) && return _depwarn_for_trailing_indices(IA)
    return true
end
function _depwarn_for_trailing_indices(n::Integer) # Called by the C boundscheck
    depwarn("omitting indices for non-singleton trailing dimensions is deprecated. Add `1`s as trailing indices or use `reshape(A, Val($n))` to make the dimensionality of the array match the number of indices.", (:getindex, :setindex!, :view))
    true
end
function _depwarn_for_trailing_indices(t::Tuple)
    depwarn("omitting indices for non-singleton trailing dimensions is deprecated. Add `$(join(map(first, t),','))` as trailing indices or use `reshape` to make the dimensionality of the array match the number of indices.", (:getindex, :setindex!, :view))
    true
end

# issue #22791
@deprecate select partialsort
@deprecate select! partialsort!
@deprecate selectperm partialsortperm
@deprecate selectperm! partialsortperm!

# `initialized` keyword arg to `sort` is deprecated in sort.jl

@deprecate promote_noncircular promote false

import .Iterators.enumerate

@deprecate enumerate(i::IndexLinear,    A::AbstractArray)  pairs(i, A)
@deprecate enumerate(i::IndexCartesian, A::AbstractArray)  pairs(i, A)

@deprecate_binding Range AbstractRange

# issue #5794
@deprecate map(f, d::T) where {T<:AbstractDict}  T( f(p) for p in pairs(d) )

# issue #17086
@deprecate isleaftype isconcretetype
@deprecate isabstract isabstracttype

# PR #22932
@deprecate +(a::Number, b::AbstractArray) broadcast(+, a, b)
@deprecate +(a::AbstractArray, b::Number) broadcast(+, a, b)
@deprecate -(a::Number, b::AbstractArray) broadcast(-, a, b)
@deprecate -(a::AbstractArray, b::Number) broadcast(-, a, b)

@deprecate(ind2sub(dims::NTuple{N,Integer}, idx::CartesianIndex{N}) where N, Tuple(idx))

@deprecate contains(eq::Function, itr, x) any(y->eq(y,x), itr)

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

export tic, toq, toc
function tic()
    depwarn("`tic()` is deprecated, use `@time`, `@elapsed`, or calls to `time_ns()` instead.", :tic)
    t0 = time_ns()
    task_local_storage(:TIMERS, (t0, get(task_local_storage(), :TIMERS, ())))
    return t0
end

function _toq()
    t1 = time_ns()
    timers = get(task_local_storage(), :TIMERS, ())
    if timers === ()
        error("`toc()` without `tic()`")
    end
    t0 = timers[1]::UInt64
    task_local_storage(:TIMERS, timers[2])
    (t1-t0)/1e9
end

function toq()
    depwarn("`toq()` is deprecated, use `@elapsed` or calls to `time_ns()` instead.", :toq)
    return _toq()
end

function toc()
    depwarn("`toc()` is deprecated, use `@time`, `@elapsed`, or calls to `time_ns()` instead.", :toc)
    t = _toq()
    println("elapsed time: ", t, " seconds")
    return t
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
    if Base.Broadcast.BroadcastStyle(typeof(x)) isa Broadcast.Unknown
        depwarn("""
            broadcast will default to iterating over its arguments in the future. Wrap arguments of
            type `x::$(typeof(x))` with `Ref(x)` to ensure they broadcast as "scalar" elements.
            """, (:broadcast, :broadcast!))
        return Ref{typeof(x)}(x)
    else
        return x
    end
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
        "To replace `full(A)`, as appropriate consider dismabiguating with a concrete ",
        "array constructor (e.g. `Array(A)`), with an abstract array constructor (e.g.`AbstractArray(A)`), ",
        "instead `convert`ing to an array type (e.g `convert(Array, A)`, `convert(AbstractArray, A)`), ",
        "or using another such operation that addresses your specific use case."),  :full)
    return A
end

# issue #20816
@deprecate strwidth textwidth
@deprecate charwidth textwidth

@deprecate find(x::Number)            findall(!iszero, x)
@deprecate findnext(A, v, i::Integer) coalesce(findnext(isequal(v), A, i), 0)
@deprecate findfirst(A, v)            coalesce(findfirst(isequal(v), A), 0)
@deprecate findprev(A, v, i::Integer) coalesce(findprev(isequal(v), A, i), 0)
@deprecate findlast(A, v)             coalesce(findlast(isequal(v), A), 0)
# to fix ambiguities introduced by deprecations
findnext(pred::Function, A, i::Integer) = invoke(findnext, Tuple{Function, Any, Any}, pred, A, i)
findprev(pred::Function, A, i::Integer) = invoke(findprev, Tuple{Function, Any, Any}, pred, A, i)
# also remove deprecation warnings in find* functions in array.jl, sparse/sparsematrix.jl,
# and sparse/sparsevector.jl.

# issue #22849
@deprecate reinterpret(::Type{T}, a::Array{S}, dims::NTuple{N,Int}) where {T, S, N} reshape(reinterpret(T, vec(a)), dims)
@deprecate reinterpret(::Type{T}, a::ReinterpretArray{S}, dims::NTuple{N,Int}) where {T, S, N} reshape(reinterpret(T, vec(a)), dims)

# issue #24006
@deprecate linearindices(s::AbstractString) eachindex(s)

# deprecate Array(shape...)-like constructors to Array(undef, shape...) equivalents
# --> former primitive constructors
@deprecate Array{T,1}(m::Int) where {T}                      Array{T,1}(undef, m)
@deprecate Array{T,2}(m::Int, n::Int) where {T}              Array{T,2}(undef, m, n)
@deprecate Array{T,3}(m::Int, n::Int, o::Int) where {T}      Array{T,3}(undef, m, n, o)
@deprecate Array{T,N}(d::Vararg{Int,N}) where {T,N}          Array{T,N}(undef, d)
@deprecate Array{T,N}(d::NTuple{N,Int}) where {T,N}          Array{T,N}(undef, d)
@deprecate Array{T}(m::Int) where {T}                        Array{T}(undef, m)
@deprecate Array{T}(m::Int, n::Int) where {T}                Array{T}(undef, m, n)
@deprecate Array{T}(m::Int, n::Int, o::Int) where {T}        Array{T}(undef, m, n, o)
@deprecate Array{T}(d::NTuple{N,Int}) where {T,N}            Array{T}(undef, d)
# --> former convenience constructors
@deprecate Vector{T}(m::Integer) where {T}                          Vector{T}(undef, m)
@deprecate Matrix{T}(m::Integer, n::Integer) where {T}              Matrix{T}(undef, m, n)
@deprecate Array{T}(m::Integer) where {T}                           Array{T}(undef, m)
@deprecate Array{T}(m::Integer, n::Integer) where {T}               Array{T}(undef, m, n)
@deprecate Array{T}(m::Integer, n::Integer, o::Integer) where {T}   Array{T}(undef, m, n, o)
@deprecate Array{T}(d::Integer...) where {T}                        Array{T}(undef, d)
@deprecate Vector(m::Integer)                                       Vector(undef, m)
@deprecate Matrix(m::Integer, n::Integer)                           Matrix(undef, m, n)

# deprecate IntSet to BitSet
@deprecate_binding IntSet BitSet

# Issue 24219
@deprecate float(x::AbstractString) parse(Float64, x)
@deprecate float(a::AbstractArray{<:AbstractString}) parse.(Float64, a)

# deprecate bits to bitstring (#24263, #24281)
@deprecate bits bitstring

# issue #24167
@deprecate EnvHash EnvDict

# issue #24349
@deprecate parse(str::AbstractString; kwargs...) Meta.parse(str; kwargs...)
@deprecate parse(str::AbstractString, pos::Int, ; kwargs...) Meta.parse(str, pos; kwargs...)
@deprecate_binding ParseError Meta.ParseError

# issue #20899
# TODO: delete JULIA_HOME deprecation in src/init.c

# cumsum and cumprod have deprecations in multidimensional.jl
# when the message is removed, the `dims` keyword argument should become required.

# issue #16307
@deprecate finalizer(o, f::Function) finalizer(f, o)
# This misses other callables but they are very rare in the wild
@deprecate finalizer(o, f::Ptr{Cvoid}) finalizer(f, o)

# Avoid ambiguity, can remove when deprecations are removed:
# This is almost certainly going to be a silent failure for code that is not updated.
finalizer(f::Ptr{Cvoid}, o::Ptr{Cvoid}) = invoke(finalizer, Tuple{Ptr{Cvoid}, Any}, f, o)
finalizer(f::Ptr{Cvoid}, o::Function) = invoke(finalizer, Tuple{Ptr{Cvoid}, Any}, f, o)

# Broadcast extension API (#23939)
@eval Broadcast begin
    Base.@deprecate_binding containertype combine_styles false
    Base.@deprecate_binding _containertype BroadcastStyle false
    Base.@deprecate_binding promote_containertype BroadcastStyle false
    Base.@deprecate_binding broadcast_c! broadcast! false ", `broadcast_c!(f, ::Type, ::Type, C, As...)` should become `broadcast!(f, C, As...)` (see the manual chapter Interfaces)"
    Base.@deprecate_binding broadcast_c broadcast false ", `broadcast_c(f, ::Type{C}, As...)` should become `broadcast(f, C, nothing, nothing, As...))` (see the manual chapter Interfaces)"
    Base.@deprecate_binding broadcast_t broadcast false ", `broadcast_t(f, ::Type{ElType}, shape, iter, As...)` should become `broadcast(f, Broadcast.DefaultArrayStyle{N}(), ElType, shape, As...))` (see the manual chapter Interfaces)"
end


### deprecations for lazier, less jazzy linalg transition in the next several blocks ###
# TODOs re. .' deprecation
#   (1) remove .' deprecation from src/julia-syntax.scm around line 2346
#   (2) remove .' documentation from base/docs/basedocs.jl around line 255
#   (3) remove .'-involving code from base/show.jl around line 1277
#   (4) remove .'-involving test from test/deprecation_exec.jl around line 178
#   (5) remove .'-related code from src/ast.scm and src/julia-parser.scm

# A[ct]_(mul|ldiv|rdiv)_B[ct][!] methods from base/operators.jl, to deprecate
@deprecate Ac_ldiv_Bt(a,b)  (\)(adjoint(a), transpose(b))
@deprecate At_ldiv_Bt(a,b)  (\)(transpose(a), transpose(b))
@deprecate A_ldiv_Bt(a,b)   (\)(a, transpose(b))
@deprecate At_ldiv_B(a,b)   (\)(transpose(a), b)
@deprecate Ac_ldiv_Bc(a,b)  (\)(adjoint(a), adjoint(b))
@deprecate A_ldiv_Bc(a,b)   (\)(a, adjoint(b))
@deprecate Ac_ldiv_B(a,b)   (\)(adjoint(a), b)
@deprecate At_rdiv_Bt(a,b)  (/)(transpose(a), transpose(b))
@deprecate A_rdiv_Bt(a,b)   (/)(a, transpose(b))
@deprecate At_rdiv_B(a,b)   (/)(transpose(a), b)
@deprecate Ac_rdiv_Bc(a,b)  (/)(adjoint(a), adjoint(b))
@deprecate A_rdiv_Bc(a,b)   (/)(a, adjoint(b))
@deprecate Ac_rdiv_B(a,b)   (/)(adjoint(a), b)
@deprecate At_mul_Bt(a,b)   (*)(transpose(a), transpose(b))
@deprecate A_mul_Bt(a,b)    (*)(a, transpose(b))
@deprecate At_mul_B(a,b)    (*)(transpose(a), b)
@deprecate Ac_mul_Bc(a,b)   (*)(adjoint(a), adjoint(b))
@deprecate A_mul_Bc(a,b)    (*)(a, adjoint(b))
@deprecate Ac_mul_B(a,b)    (*)(adjoint(a), b)

# issue #24822
@deprecate_binding Display AbstractDisplay

# 24595
@deprecate falses(A::AbstractArray) falses(size(A))
@deprecate trues(A::AbstractArray) trues(size(A))

# issue #24794
@deprecate linspace(start, stop)     range(start, stop=stop, length=50)
@deprecate logspace(start, stop)     exp10.(range(start, stop=stop, length=50))

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

# issue #25082
@deprecate_binding Void Nothing

# #24844
@deprecate copy!(dest::AbstractSet, src) union!(dest, src)

function copy!(dest::AbstractSet, src::AbstractSet)
    depwarn("`copy!(dst::AbstractSet, src::AbstractSet)` is deprecated. " *
            "You can either use `union!(dst, src)` or `Future.copy!(dst, src)` instead.", :copy!)
    union!(dest, src)
end

@deprecate copy!(dest::AbstractDict, src) foldl(push!, dest, src)

function copy!(dest::AbstractDict, src::AbstractDict)
    depwarn("`copy!(dst::AbstractDict, src::AbstractDict)` is deprecated. " *
            "You can either use `merge!(dst, src)` or `Future.copy!(dst, src)` instead.", :copy!)
    foldl(push!, dest, src)
end

# 24808
@deprecate copy!(dest::Union{AbstractArray,IndexStyle}, args...) copyto!(dest, args...)

function copy!(dest::AbstractArray, src::AbstractArray)
    depwarn("`copy!(dst::AbstractArray, src::AbstractArray)` is deprecated. " *
            "You can either use `copyto!(dst, src)` or `Future.copy!(dst, src)` instead.", :copy!)
    copyto!(dest, src)
end

@deprecate unsafe_copy!(dest, args...) unsafe_copyto!(dest, args...)

# issue #24019
@deprecate similar(a::AbstractDict) empty(a)
@deprecate similar(a::AbstractDict, ::Type{Pair{K,V}}) where {K, V} empty(a, K, V)

# 25224
@deprecate similar(s::AbstractSet) empty(s)
@deprecate similar(s::AbstractSet, ::Type{T}) where {T} empty(s, T)

# issue #24804
@deprecate_moved sum_kbn "KahanSummation"
@deprecate_moved cumsum_kbn "KahanSummation"

@deprecate isalnum(c::Char) isalpha(c) || isnumeric(c)
@deprecate isgraph(c::Char) isprint(c) && !isspace(c)
@deprecate isnumber(c::Char) isnumeric(c)

# PR #24647
@deprecate_binding Complex32  ComplexF16
@deprecate_binding Complex64  ComplexF32
@deprecate_binding Complex128 ComplexF64

# PR #24999
@deprecate ind2chr(s::AbstractString, i::Integer) length(s, 1, i)
@deprecate chr2ind(s::AbstractString, n::Integer) nextind(s, 0, n)

# Associative -> AbstractDict (#25012)
@deprecate_binding Associative AbstractDict

@deprecate_binding KeyIterator KeySet false

# issue #25016
@deprecate lpad(s, n::Integer, p) lpad(string(s), n, string(p))
@deprecate rpad(s, n::Integer, p) rpad(string(s), n, string(p))

# PR #25011
@deprecate push!(env::EnvDict, k::AbstractString, v) push!(env, k=>v)

# issue #24868
@deprecate sprint(size::Integer, f::Function, args...; env=nothing) sprint(f, args...; context=env, sizehint=size)

# PR #25057
@deprecate indices(a) axes(a)
@deprecate indices(a, d) axes(a, d)

# PR #25046
export reload, workspace
reload(name::AbstractString) = error("`reload($(repr(name)))` is discontinued, consider Revise.jl for an alternative workflow.")
workspace() = error("`workspace()` is discontinued, consider Revise.jl for an alternative workflow.")

# Issue #12902
@deprecate parentindexes parentindices

# Issue #23902
@deprecate unshift! pushfirst!
@deprecate shift! popfirst!

# Issue #23642
@deprecate_moved Nullable "Nullables"
@deprecate_moved NullException "Nullables"
@deprecate_moved isnull "Nullables"
@deprecate_moved unsafe_get "Nullables"

# sub2ind and ind2sub deprecation (PR #24715)
@deprecate ind2sub(A::AbstractArray, ind) CartesianIndices(A)[ind]
@deprecate ind2sub(::Tuple{}, ind::Integer) CartesianIndices()[ind]
@deprecate ind2sub(dims::Tuple{Vararg{Integer,N}} where N, ind::Integer) CartesianIndices(dims)[ind]
@deprecate ind2sub(inds::Tuple{Base.OneTo}, ind::Integer) CartesianIndices(inds)[ind]
@deprecate ind2sub(inds::Tuple{AbstractUnitRange}, ind::Integer) CartesianIndices(inds)[ind]
@deprecate ind2sub(inds::Tuple{Vararg{AbstractUnitRange,N}} where N, ind::Integer) CartesianIndices(inds)[ind]
@deprecate ind2sub(inds::Union{DimsInteger{N},Indices{N}}  where N, ind::AbstractVector{<:Integer}) CartesianIndices(inds)[ind]

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


@deprecate lexcmp(x::AbstractArray, y::AbstractArray) cmp(x, y)
@deprecate lexcmp(x::Real, y::Real)                   cmp(isless, x, y)
@deprecate lexcmp(x::Complex, y::Complex)             cmp((real(x),imag(x)), (real(y),imag(y)))
@deprecate lexcmp(x, y)                               cmp(x, y)

@deprecate lexless isless

@deprecate(
    open(filename::AbstractString, read::Bool, write::Bool, create::Bool, truncate::Bool, append::Bool),
    open(filename, read = read, write = write, create = create, truncate = truncate, append = append)
)
@deprecate(
    open(f::Function, filename::AbstractString, read::Bool, write::Bool, create::Bool, truncate::Bool, append::Bool),
    open(f, filename, read = read, write = write, create = create, truncate = truncate, append = append)
)

@deprecate_binding iteratorsize IteratorSize
@deprecate_binding iteratoreltype IteratorEltype

# issue #25440
@deprecate_binding TypeOrder           OrderStyle
@deprecate_binding TypeArithmetic      ArithmeticStyle
@deprecate_binding TypeRangeStep       RangeStepStyle
@deprecate_binding HasOrder            Ordered
@deprecate_binding ArithmeticOverflows ArithmeticWraps

@deprecate search(str::Union{String,SubString}, re::Regex, idx::Integer) coalesce(findnext(re, str, idx), 0:-1)
@deprecate search(s::AbstractString, r::Regex, idx::Integer) coalesce(findnext(r, s, idx), 0:-1)
@deprecate search(s::AbstractString, r::Regex) coalesce(findfirst(r, s), 0:-1)
@deprecate search(s::AbstractString, c::Char, i::Integer) coalesce(findnext(isequal(c), s, i), 0)
@deprecate search(s::AbstractString, c::Char) coalesce(findfirst(isequal(c), s), 0)
@deprecate search(a::ByteArray, b::Union{Int8,UInt8}, i::Integer) coalesce(findnext(isequal(b), a, i), 0)
@deprecate search(a::ByteArray, b::Union{Int8,UInt8}) coalesce(findfirst(isequal(b), a), 0)
@deprecate search(a::String, b::Union{Int8,UInt8}, i::Integer) coalesce(findnext(isequal(b), unsafe_wrap(Vector{UInt8}, a), i), 0)
@deprecate search(a::String, b::Union{Int8,UInt8}) coalesce(findfirst(isequal(b), unsafe_wrap(Vector{UInt8}, a)), 0)
@deprecate search(a::ByteArray, b::Char, i::Integer) coalesce(findnext(isequal(UInt8(b)), a, i), 0)
@deprecate search(a::ByteArray, b::Char) coalesce(findfirst(isequal(UInt8(b)), a), 0)

@deprecate search(s::AbstractString, c::Union{Tuple{Vararg{Char}},AbstractVector{Char},Set{Char}}, i::Integer) coalesce(findnext(in(c), s, i), 0)
@deprecate search(s::AbstractString, c::Union{Tuple{Vararg{Char}},AbstractVector{Char},Set{Char}}) coalesce(findfirst(in(c), s), 0)
@deprecate search(s::AbstractString, t::AbstractString, i::Integer) coalesce(findnext(t, s, i), 0:-1)
@deprecate search(s::AbstractString, t::AbstractString) coalesce(findfirst(t, s), 0:-1)

@deprecate rsearch(s::AbstractString, c::Union{Tuple{Vararg{Char}},AbstractVector{Char},Set{Char}}, i::Integer) coalesce(findprev(in(c), s, i), 0)
@deprecate rsearch(s::AbstractString, c::Union{Tuple{Vararg{Char}},AbstractVector{Char},Set{Char}}) coalesce(findlast(in(c), s), 0)
@deprecate rsearch(s::AbstractString, t::AbstractString, i::Integer) coalesce(findprev(t, s, i), 0:-1)
@deprecate rsearch(s::AbstractString, t::AbstractString) coalesce(findlast(t, s), 0:-1)

@deprecate rsearch(str::Union{String,SubString}, re::Regex, idx::Integer) coalesce(findprev(re, str, idx), 0:-1)
@deprecate rsearch(str::Union{String,SubString}, re::Regex) coalesce(findlast(re, str), 0:-1)
@deprecate rsearch(s::AbstractString, r::Regex, idx::Integer) coalesce(findprev(r, s, idx), 0:-1)
@deprecate rsearch(s::AbstractString, r::Regex) coalesce(findlast(r, s), 0:-1)
@deprecate rsearch(s::AbstractString, c::Char, i::Integer) coalesce(findprev(isequal(c), s, i), 0)
@deprecate rsearch(s::AbstractString, c::Char) coalesce(findlast(isequal(c), s), 0)
@deprecate rsearch(a::Union{String,ByteArray}, b::Union{Int8,UInt8}, i::Integer = lastindex(a)) coalesce(findprev(isequal(b), a, i), 0)
@deprecate rsearch(a::String, b::Union{Int8,UInt8}, i::Integer = lastindex(a)) coalesce(findprev(isequal(Char(b)), a, i), 0)
@deprecate rsearch(a::ByteArray, b::Char, i::Integer = lastindex(a)) coalesce(findprev(isequal(UInt8(b)), a, i), 0)

@deprecate searchindex(s::AbstractString, t::AbstractString) first(coalesce(findfirst(t, s), 0:-1))
@deprecate searchindex(s::AbstractString, t::AbstractString, i::Integer) first(coalesce(findnext(t, s, i), 0:-1))
@deprecate rsearchindex(s::AbstractString, t::AbstractString) first(coalesce(findlast(t, s), 0:-1))
@deprecate rsearchindex(s::AbstractString, t::AbstractString, i::Integer) first(coalesce(findprev(t, s, i), 0:-1))

@deprecate searchindex(s::AbstractString, c::Char) coalesce(findfirst(isequal(c), s), 0)
@deprecate searchindex(s::AbstractString, c::Char, i::Integer) coalesce(findnext(isequal(c), s, i), 0)
@deprecate rsearchindex(s::AbstractString, c::Char) coalesce(findlast(isequal(c), s), 0)
@deprecate rsearchindex(s::AbstractString, c::Char, i::Integer) coalesce(findprev(isequal(c), s, i), 0)

@deprecate ismatch(r::Regex, s::AbstractString) occursin(r, s)

@deprecate findin(a, b) findall(in(b), a)

@deprecate find findall
@deprecate find(A::AbstractVector) findall(A)
@deprecate find(A::AbstractArray) LinearIndices(A)[findall(A)]
@deprecate find(f::Function, A::AbstractVector) findall(f, A)
@deprecate find(f::Function, A::AbstractArray) LinearIndices(A)[findall(f, A)]

@deprecate findn(x::AbstractVector) (findall(!iszero, x),)
@deprecate findn(x::AbstractMatrix) (I = findall(!iszero, x); (getindex.(I, 1), getindex.(I, 2)))
@deprecate findn(x::AbstractArray{T, N}) where {T, N} (I = findall(!iszero, x); ntuple(i -> getindex.(I, i), N))

@deprecate catch_stacktrace(c_funcs::Bool)  stacktrace(catch_backtrace(), c_funcs)
@deprecate catch_stacktrace()               stacktrace(catch_backtrace())

@deprecate method_exists(f, t)        hasmethod(f, t)
@deprecate method_exists(f, t, world) hasmethod(f, t, world = world)

@deprecate object_id objectid

@deprecate gc GC.gc
@deprecate gc_enable GC.enable
@eval @deprecate $(Symbol("@gc_preserve")) GC.$(Symbol("@preserve")) false

@deprecate nb_available bytesavailable

@deprecate skipchars(io::IO, predicate; linecomment=nothing) skipchars(predicate, io, linecomment=linecomment)
# this method is to avoid ambiguity, delete at the same time as deprecation of skipchars above:
skipchars(::IO, ::IO; linecomment=nothing) = throw(ArgumentError("the first argument of `skipchars` must be callable"))

# Issue #25745
@deprecate print_shortest Base.Grisu.print_shortest

# issue #9053
if Sys.iswindows()
function Filesystem.tempname(uunique::UInt32)
    error("`tempname(::UInt32)` is discontinued.")
end
end

"""
    readandwrite(command)

Starts running a command asynchronously, and returns a tuple (stdout,stdin,process) of the
output stream and input stream of the process, and the process object itself.
"""
function readandwrite(cmds::AbstractCmd)
    depwarn("""`readandwrite(::Cmd)` is deprecated in favor of `open(::Cmd, \"r+\").
               You may read/write the returned process object for access to stdio.""",
            :readandwrite)
    processes = open(cmds, "r+")
    return (processes.out, processes.in, processes)
end
export readandwrite

@deprecate module_parent(m::Module) parentmodule(m)
@deprecate datatype_module(t::DataType) parentmodule(t) false
@deprecate datatype_module(t::UnionAll) parentmodule(t) false
@deprecate function_module(f::Function) parentmodule(f) false
@deprecate function_module(f, t) parentmodule(f, t) false

# PR #25622
@deprecate module_name(m::Module) nameof(m)
@deprecate function_name(f::Function) nameof(f) false
@deprecate datatype_name(t::DataType) nameof(t) false
@deprecate datatype_name(t::UnionAll) nameof(t) false

# issue #25501
@deprecate sum(a::AbstractArray, dims)        sum(a, dims=dims)
@deprecate sum(f, a::AbstractArray, dims)     sum(f, a, dims=dims)
@deprecate prod(a::AbstractArray, dims)       prod(a, dims=dims)
@deprecate prod(f, a::AbstractArray, dims)    prod(f, a, dims=dims)
@deprecate maximum(a::AbstractArray, dims)    maximum(a, dims=dims)
@deprecate maximum(f, a::AbstractArray, dims) maximum(f, a, dims=dims)
@deprecate minimum(a::AbstractArray, dims)    minimum(a, dims=dims)
@deprecate minimum(f, a::AbstractArray, dims) minimum(f, a, dims=dims)
@deprecate all(a::AbstractArray, dims)        all(a, dims=dims)
@deprecate all(f, a::AbstractArray, dims)     all(f, a, dims=dims)
@deprecate any(a::AbstractArray, dims)        any(a, dims=dims)
@deprecate any(f, a::AbstractArray, dims)     any(f, a, dims=dims)
@deprecate findmax(A::AbstractArray, dims)    findmax(A, dims=dims)
@deprecate findmin(A::AbstractArray, dims)    findmin(A, dims=dims)

@deprecate mean(A::AbstractArray, dims)                              mean(A, dims=dims)
@deprecate varm(A::AbstractArray, m::AbstractArray, dims; kwargs...) varm(A, m; kwargs..., dims=dims)
@deprecate var(A::AbstractArray, dims; kwargs...)                    var(A; kwargs..., dims=dims)
@deprecate std(A::AbstractArray, dims; kwargs...)                    std(A; kwargs..., dims=dims)
@deprecate cov(X::AbstractMatrix, dim::Int; kwargs...)               cov(X; kwargs..., dims=dim)
@deprecate cov(x::AbstractVecOrMat, y::AbstractVecOrMat, dim::Int; kwargs...) cov(x, y; kwargs..., dims=dim)
@deprecate cor(X::AbstractMatrix, dim::Int)                          cor(X, dims=dim)
@deprecate cor(x::AbstractVecOrMat, y::AbstractVecOrMat, dim::Int)   cor(x, y, dims=dim)
@deprecate median(A::AbstractArray, dims; kwargs...)                 median(A; kwargs..., dims=dims)

@deprecate mapreducedim(f, op, A::AbstractArray, dims)     mapreduce(f, op, A, dims=dims)
@deprecate mapreducedim(f, op, A::AbstractArray, dims, v0) mapreduce(f, op, v0, A, dims=dims)
@deprecate reducedim(op, A::AbstractArray, dims)           reduce(op, A, dims=dims)
@deprecate reducedim(op, A::AbstractArray, dims, v0)       reduce(op, v0, A, dims=dims)

@deprecate sort(A::AbstractArray, dim::Integer; kwargs...) sort(A; kwargs..., dims=dim)

@deprecate accumulate(op, A, dim::Integer)               accumulate(op, A, dims=dim)
@deprecate accumulate!(op, B, A, dim::Integer)           accumulate!(op, B, A, dims=dim)
@deprecate cumsum(A::AbstractArray, dim::Integer)        cumsum(A, dims=dim)
@deprecate cumsum!(B, A, dim::Integer)                   cumsum!(B, A, dims=dim)
@deprecate cumsum!(out, A::AbstractVector, dim::Integer) cumsum!(out, A, dims=dim)
@deprecate cumprod(A::AbstractArray, dim::Integer)       cumprod(A, dims=dim)
@deprecate cumprod!(B, A, dim::Integer)                  cumprod!(B, A, dims=dim)

@deprecate flipdim(A, d) reverse(A, dims=d)

# PR #25196
@deprecate_binding ObjectIdDict IdDict{Any,Any}

@deprecate quit() exit()

# PR #25654
@deprecate indmin argmin
@deprecate indmax argmax

# PR #25896
@deprecate range(start, length) range(start, length=length)
@deprecate range(start, step, length) range(start, step=step, length=length)
@deprecate linspace(start, stop, length::Integer) range(start, stop=stop, length=length)
@deprecate linspace(start, stop, length::Real) range(start, stop=stop, length=Int(length))
@deprecate_binding LinSpace LinRange
@deprecate logspace(start, stop, n; base=10) base.^range(start, stop=stop, length=n)

@deprecate runtests(tests, ncores; kw...) runtests(tests; ncores = ncores, kw...) false
@deprecate code_lowered(f, types, generated) code_lowered(f, types, generated = generated)

# PR 25458
@deprecate endof(a) lastindex(a)
function firstindex(a)
    depwarn("if appropriate you should implement `firstindex` for type $(typeof(a)), which might just return 1", :firstindex)
    1
end

# PR 25763
function lastindex(a, n)
    depwarn("if appropriate you should implement `lastindex(a, n)` for type $(typeof(a))`, which might just return `last(axes(a, n))`", :lastindex)
    last(axes(a, n))
end

@deprecate_binding repmat repeat

@deprecate Timer(timeout, repeat) Timer(timeout, interval = repeat)
@deprecate Timer(callback, delay, repeat) Timer(callback, delay, interval = repeat)
@deprecate names(m, all) names(m, all = all)
@deprecate names(m, all, imported) names(m, all = all, imported = imported)
@deprecate eachmatch(re, str, overlap) eachmatch(re, str, overlap = overlap)
@deprecate matchall(re, str, overlap) collect(m.match for m in eachmatch(re, str, overlap = overlap))
@deprecate chop(s, head) chop(s, head = head)
@deprecate chop(s, head, tail) chop(s, head = head, tail = tail)
@deprecate tryparse(T::Type{<:Integer}, s, base) tryparse(T, s, base = base)
@deprecate parse(T::Type{<:Integer}, s, base) parse(T, s, base = base)
@eval Filesystem @deprecate mkdir(path, mode) mkdir(path, mode = mode)
@eval Filesystem @deprecate mkpath(path, mode) mkpath(path, mode = mode)
@deprecate countlines(x, eol) countlines(x, eol = eol)
@deprecate PipeBuffer(data, maxsize) PipeBuffer(data, maxsize = maxsize)
@deprecate unsafe_wrap(T, pointer, dims, own) unsafe_wrap(T, pointer, dims, own = own)
@deprecate digits(n, base, pad) digits(n, base = base, pad = pad)
@deprecate digits(T, n, base, pad) digits(T, n, base = base, pad = pad)

@deprecate print_with_color(color, args...; kwargs...) printstyled(args...; kwargs..., color=color)

@deprecate base(b, n)      string(n, base = b)
@deprecate base(b, n, pad) string(n, base = b, pad = pad)
@deprecate bin(n)          string(n, base = 2)
@deprecate bin(n, pad)     string(n, base = 2, pad = pad)
@deprecate oct(n)          string(n, base = 8)
@deprecate oct(n, pad)     string(n, base = 8, pad = pad)
@deprecate dec(n)          string(n)
@deprecate dec(n, pad)     string(n, pad = pad)
@deprecate hex(n)          string(n, base = 16)
@deprecate hex(n, pad)     string(n, base = 16, pad = pad)
@deprecate bin(n::Char)      string(UInt32(n), base = 2)
@deprecate bin(n::Char, pad) string(UInt32(n), base = 2, pad = pad)
@deprecate oct(n::Char)      string(UInt32(n), base = 8)
@deprecate oct(n::Char, pad) string(UInt32(n), base = 8, pad = pad)
@deprecate dec(n::Char)      string(UInt32(n))
@deprecate dec(n::Char, pad) string(UInt32(n), pad = pad)
@deprecate hex(n::Char)      string(UInt32(n), base = 16)
@deprecate hex(n::Char, pad) string(UInt32(n), base = 16, pad = pad)

@deprecate which(s::Symbol) which(Main, s)

@deprecate IOBuffer(data::AbstractVector{UInt8}, read::Bool, write::Bool=false, maxsize::Integer=typemax(Int)) IOBuffer(data, read=read, write=write, maxsize=maxsize)
@deprecate IOBuffer(read::Bool, write::Bool) IOBuffer(read=read, write=write)
@deprecate IOBuffer(maxsize::Integer) IOBuffer(read=true, write=true, maxsize=maxsize)

@deprecate reprmime(mime, x) repr(mime, x)
@deprecate mimewritable(mime, x) showable(mime, x)

# PR #26284
@deprecate (+)(i::Integer, index::CartesianIndex) (i*one(index) + index)
@deprecate (+)(index::CartesianIndex, i::Integer) (index + i*one(index))
@deprecate (-)(i::Integer, index::CartesianIndex) (i*one(index) - index)
@deprecate (-)(index::CartesianIndex, i::Integer) (index - i*one(index))

# PR #23332
@deprecate ^(x, p::Integer) Base.power_by_squaring(x,p)

# Issue #25979
# The `remove_destination` keyword to `cp`, `mv`, and the unexported `cptree` has been
# renamed to `force`. To remove this deprecation, remove the `remove_destination` keyword
# argument from the function signatures as well as the internal logic that deals with the
# renaming. These live in base/file.jl.

# issue #25928
@deprecate wait(t::Task) fetch(t)

# issue #18326
@deprecate slicedim(A::AbstractArray, d::Integer, i) copy(selectdim(A, d, i))
@deprecate slicedim(A::BitVector, d::Integer, i::Number) copy(selectdim(A, d, i))
function slicedim(A::AbstractVector, d::Integer, i::Number)
    if d == 1
        # slicedim would have returned a scalar value, selectdim always returns views
        depwarn("`slicedim(A::AbstractVector, d::Integer, i)` is deprecated, use `selectdim(A, d, i)[]` instead.", :slicedim)
        selectdim(A, d, i)[]
    else
        depwarn("`slicedim(A::AbstractArray, d::Integer, i)` is deprecated, use `copy(selectdim(A, d, i))` instead.", :slicedim)
        copy(selectdim(A, d, i))
    end
end

# PR #26283
@deprecate contains(haystack, needle) occursin(needle, haystack)
@deprecate contains(s::AbstractString, r::Regex, offset::Integer) occursin(r, s, offset=offset)
function (r::Regex)(s)
    depwarn("`(r::Regex)(s)` is deprecated, use `occursin(r, s)` instead.", :Regex)
    occursin(r, s)
end

# Issue #25786
@deprecate_binding DevNull devnull
# TODO: When these are removed, also remove the uppercase variants in libuv.jl and stream.jl
@deprecate_binding STDIN stdin true nothing false
@deprecate_binding STDOUT stdout true nothing false
@deprecate_binding STDERR stderr true nothing false

# PR 25062
@deprecate(link_pipe(pipe; julia_only_read = true, julia_only_write = true),
           link_pipe!(pipe, reader_supports_async = julia_only_read, writer_supports_async = julia_only_write),
           false)

# PR 26156
@deprecate trunc(x, digits, base) trunc(x, digits, base = base)
@deprecate floor(x, digits, base) floor(x, digits, base = base)
@deprecate ceil(x, digits, base) ceil(x, digits, base = base)
@deprecate round(x, digits, base) round(x, digits, base = base)
@deprecate signif(x, digits, base) signif(x, digits, base = base)

# issue #25965
@deprecate spawn(cmds::AbstractCmd) run(cmds, wait = false)

# Remember to delete the module when removing this
@eval Base.Math module JuliaLibm
    Base.@deprecate log Base.log
end

# PR 26071
@deprecate(matchall(r::Regex, s::AbstractString; overlap::Bool = false),
           collect(m.match for m in eachmatch(r, s, overlap = overlap)))

@deprecate diff(A::AbstractMatrix) diff(A, 1)

# PR 26194
export assert
function assert(x)
    depwarn("`assert` is deprecated, use `@assert` instead.", :assert)
    @assert x ""
end

# Issue #26248
@deprecate conj(x) x

# PR #26436
@deprecate equalto(x) isequal(x)
@deprecate(occursin(x), in(x))
@deprecate_binding EqualTo Base.Fix2{typeof(isequal)} false
@deprecate_binding OccursIn Base.Fix2{typeof(in)} false

# Remove ambiguous CartesianIndices and LinearIndices constructors that are ambiguous between an axis and an array (#26448)
@eval IteratorsMD @deprecate CartesianIndices(inds::Vararg{AbstractUnitRange{Int},N}) where {N} CartesianIndices(inds)
@eval IteratorsMD @deprecate CartesianIndices(inds::Vararg{AbstractUnitRange{<:Integer},N}) where {N} CartesianIndices(inds)
@eval IteratorsMD @deprecate LinearIndices(inds::Vararg{AbstractUnitRange{Int},N}) where {N} LinearIndices(inds)
@eval IteratorsMD @deprecate LinearIndices(inds::Vararg{AbstractUnitRange{<:Integer},N}) where {N} LinearIndices(inds)

# rename uninitialized
@deprecate_binding uninitialized undef
@deprecate_binding Uninitialized UndefInitializer

# remove broadcast MatrixStyle and VectorStyle (Issue #26430)
@eval Broadcast Base.@deprecate_binding MatrixStyle DefaultArrayStyle{2} false
@eval Broadcast Base.@deprecate_binding VectorStyle DefaultArrayStyle{1} false

@deprecate Crand Libc.rand false
@deprecate Csrand Libc.srand false

@deprecate showcompact(x) show(IOContext(stdout, :compact => true), x)
@deprecate showcompact(io, x) show(IOContext(io, :compact => true), x)
@deprecate sprint(::typeof(showcompact), args...) sprint(show, args...; context=:compact => true)

@deprecate isupper isuppercase
@deprecate islower islowercase
@deprecate ucfirst uppercasefirst
@deprecate lcfirst lowercasefirst

function search(buf::IOBuffer, delim::UInt8)
    Base.depwarn("search(buf::IOBuffer, delim::UInt8) is deprecated: use occursin(delim, buf) or readuntil(buf, delim) instead", :search)
    p = pointer(buf.data, buf.ptr)
    q = GC.@preserve buf ccall(:memchr,Ptr{UInt8},(Ptr{UInt8},Int32,Csize_t),p,delim,bytesavailable(buf))
    q == C_NULL && return nothing
    return Int(q-p+1)
end

# END 0.7 deprecations

# BEGIN 1.0 deprecations

# END 1.0 deprecations
