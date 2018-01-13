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

macro deprecate_binding(old, new, export_old=true, dep_message=nothing)
    dep_message == nothing && (dep_message = ", use $new instead")
    return Expr(:toplevel,
         export_old ? Expr(:export, esc(old)) : nothing,
         Expr(:const, Expr(:(=), esc(Symbol(string("_dep_message_",old))), esc(dep_message))),
         Expr(:const, Expr(:(=), esc(old), esc(new))),
         Expr(:call, :deprecate, __module__, Expr(:quote, old)))
end

macro deprecate_moved(old, new, export_old=true, default_package=false)
    eold = esc(old)
    return Expr(:toplevel,
         default_package ? :(function $eold(args...; kwargs...)
                                 error($eold, " has been moved to the standard library package ", $new, ".\n",
                                       "Restart Julia and then run `using ", $new, "` to load it.")
                             end) :
                           :(function $eold(args...; kwargs...)
                                 error($eold, " has been moved to the package ", $new, ".jl.\n",
                                       "Run `Pkg.add(\"", $new, "\")` to install it, restart Julia,\n",
                                       "and then run `using ", $new, "` to load it.")
                             end),
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

import .LinAlg: cond
@deprecate cond(F::LinAlg.LU, p::Integer) cond(convert(AbstractArray, F), p)

# PR #21359
import .Random: srand, randjump

@deprecate srand(r::MersenneTwister, filename::AbstractString, n::Integer=4) srand(r, read!(filename, Vector{UInt32}(uninitialized, Int(n))))
@deprecate srand(filename::AbstractString, n::Integer=4) srand(read!(filename, Vector{UInt32}(uninitialized, Int(n))))
@deprecate MersenneTwister(filename::AbstractString)  srand(MersenneTwister(0), read!(filename, Vector{UInt32}(uninitialized, Int(4))))

function randjump(mt::MersenneTwister, jumps::Integer, jumppoly::AbstractString)
    depwarn("`randjump(rng, jumps, jumppoly::AbstractString)` is deprecated; use `randjump(rng, steps, jumps)` instead", :randjump)
    Base.Random._randjump(mt, dSFMT.GF2X(jumppoly), jumps)
end

@deprecate randjump(mt::MersenneTwister, jumps::Integer)  randjump(mt, big(10)^20, jumps)

# PR #21974
@deprecate versioninfo(verbose::Bool) versioninfo(verbose=verbose)
@deprecate versioninfo(io::IO, verbose::Bool) versioninfo(io, verbose=verbose)

# PR #22188
import .LinAlg: cholfact, cholfact!
@deprecate cholfact!(A::StridedMatrix, uplo::Symbol, ::Type{Val{false}}) cholfact!(Hermitian(A, uplo), Val(false))
@deprecate cholfact!(A::StridedMatrix, uplo::Symbol) cholfact!(Hermitian(A, uplo))
@deprecate cholfact(A::StridedMatrix, uplo::Symbol, ::Type{Val{false}}) cholfact(Hermitian(A, uplo), Val(false))
@deprecate cholfact(A::StridedMatrix, uplo::Symbol) cholfact(Hermitian(A, uplo))
@deprecate cholfact!(A::StridedMatrix, uplo::Symbol, ::Type{Val{true}}; tol = 0.0) cholfact!(Hermitian(A, uplo), Val(true), tol = tol)
@deprecate cholfact(A::StridedMatrix, uplo::Symbol, ::Type{Val{true}}; tol = 0.0) cholfact(Hermitian(A, uplo), Val(true), tol = tol)

# PR #22245
import .LinAlg: isposdef, isposdef!
@deprecate isposdef(A::AbstractMatrix, UL::Symbol) isposdef(Hermitian(A, UL))
@deprecate isposdef!(A::StridedMatrix, UL::Symbol) isposdef!(Hermitian(A, UL))

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

# PR #22062
function LibGit2.set_remote_url(repo::LibGit2.GitRepo, url::AbstractString; remote::AbstractString="origin")
    Base.depwarn(string(
        "`LibGit2.set_remote_url(repo, url; remote=remote)` is deprecated, use ",
        "`LibGit2.set_remote_url(repo, remote, url)` instead."), :set_remote_url)
    LibGit2.set_remote_url(repo, remote, url)
end
function LibGit2.set_remote_url(path::AbstractString, url::AbstractString; remote::AbstractString="origin")
    Base.depwarn(string(
        "`LibGit2.set_remote_url(path, url; remote=remote)` is deprecated, use ",
        "`LibGit2.set_remote_url(path, remote, url)` instead."), :set_remote_url)
    LibGit2.set_remote_url(path, remote, url)
end

module Operators
    for op in [:!, :(!=), :(!==), :%, :&, :*, :+, :-, :/, ://, :<, :<:, :<<, :(<=),
               :<|, :(==), :(===), :>, :>:, :(>=), :>>, :>>>, :\, :^, :colon,
               :adjoint, :getindex, :hcat, :hvcat, :setindex!, :transpose, :vcat,
               :xor, :|, :|>, :~, :×, :÷, :∈, :∉, :∋, :∌, :∘, :√, :∛, :∩, :∪, :≠, :≤,
               :≥, :⊆, :⊈, :⊊, :⊻, :⋅]
        if isdefined(Base, op)
            @eval Base.@deprecate_binding $op Base.$op
        end
    end
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
for f in filter(s -> isexported(DFT, s), names(DFT, true))
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

@deprecate_moved SharedArray "SharedArrays" true true

@eval @deprecate_moved $(Symbol("@profile")) "Profile" true true

@deprecate_moved readdlm  "DelimitedFiles" true true
@deprecate_moved writedlm "DelimitedFiles" true true
@deprecate_moved readcsv  "DelimitedFiles" true true
@deprecate_moved writecsv "DelimitedFiles" true true

@deprecate_moved base64encode "Base64" true true
@deprecate_moved base64decode "Base64" true true
@deprecate_moved Base64EncodePipe "Base64" true true
@deprecate_moved Base64DecodePipe "Base64" true true

@deprecate_moved poll_fd "FileWatching" true true
@deprecate_moved poll_file "FileWatching" true true
@deprecate_moved PollingFileWatcher "FileWatching" true true
@deprecate_moved watch_file "FileWatching" true true
@deprecate_moved FileMonitor "FileWatching" true true

@eval @deprecate_moved $(Symbol("@spawn")) "Distributed" true true
@eval @deprecate_moved $(Symbol("@spawnat")) "Distributed" true true
@eval @deprecate_moved $(Symbol("@fetch")) "Distributed" true true
@eval @deprecate_moved $(Symbol("@fetchfrom")) "Distributed" true true
@eval @deprecate_moved $(Symbol("@everywhere")) "Distributed" true true
@eval @deprecate_moved $(Symbol("@parallel")) "Distributed" true true

@deprecate_moved addprocs "Distributed" true true
@deprecate_moved CachingPool "Distributed" true true
@deprecate_moved clear! "Distributed" true true
@deprecate_moved ClusterManager "Distributed" true true
@deprecate_moved default_worker_pool "Distributed" true true
@deprecate_moved init_worker "Distributed" true true
@deprecate_moved interrupt "Distributed" true true
@deprecate_moved launch "Distributed" true true
@deprecate_moved manage "Distributed" true true
@deprecate_moved myid "Distributed" true true
@deprecate_moved nprocs "Distributed" true true
@deprecate_moved nworkers "Distributed" true true
@deprecate_moved pmap "Distributed" true true
@deprecate_moved procs "Distributed" true true
@deprecate_moved remote "Distributed" true true
@deprecate_moved remotecall "Distributed" true true
@deprecate_moved remotecall_fetch "Distributed" true true
@deprecate_moved remotecall_wait "Distributed" true true
@deprecate_moved remote_do "Distributed" true true
@deprecate_moved rmprocs "Distributed" true true
@deprecate_moved workers "Distributed" true true
@deprecate_moved WorkerPool "Distributed" true true
@deprecate_moved RemoteChannel "Distributed" true true
@deprecate_moved Future "Distributed" true true
@deprecate_moved WorkerConfig "Distributed" true true
@deprecate_moved RemoteException "Distributed" true true
@deprecate_moved ProcessExitedException "Distributed" true true


@deprecate_moved crc32c "CRC32c" true true

@deprecate_moved DateTime "Dates" true true
@deprecate_moved DateFormat "Dates" true true
@eval @deprecate_moved $(Symbol("@dateformat_str")) "Dates" true true
@deprecate_moved now "Dates" true true

@deprecate_moved eigs "IterativeEigensolvers" true true
@deprecate_moved svds "IterativeEigensolvers" true true

@eval @deprecate_moved $(Symbol("@printf")) "Printf" true true
@eval @deprecate_moved $(Symbol("@sprintf")) "Printf" true true

# PR #21709
@deprecate cov(x::AbstractVector, corrected::Bool) cov(x, corrected=corrected)
@deprecate cov(x::AbstractMatrix, vardim::Int, corrected::Bool) cov(x, vardim, corrected=corrected)
@deprecate cov(X::AbstractVector, Y::AbstractVector, corrected::Bool) cov(X, Y, corrected=corrected)
@deprecate cov(X::AbstractVecOrMat, Y::AbstractVecOrMat, vardim::Int, corrected::Bool) cov(X, Y, vardim, corrected=corrected)

# bkfact
import .LinAlg: bkfact, bkfact!
function bkfact(A::StridedMatrix, uplo::Symbol, symmetric::Bool = issymmetric(A), rook::Bool = false)
    depwarn(string("`bkfact` with uplo and symmetric arguments is deprecated, ",
        "use `bkfact($(symmetric ? "Symmetric(" : "Hermitian(")A, :$uplo))` instead."),
        :bkfact)
    return bkfact(symmetric ? Symmetric(A, uplo) : Hermitian(A, uplo), rook)
end
function bkfact!(A::StridedMatrix, uplo::Symbol, symmetric::Bool = issymmetric(A), rook::Bool = false)
    depwarn(string("`bkfact!` with uplo and symmetric arguments is deprecated, ",
        "use `bkfact!($(symmetric ? "Symmetric(" : "Hermitian(")A, :$uplo))` instead."),
        :bkfact!)
    return bkfact!(symmetric ? Symmetric(A, uplo) : Hermitian(A, uplo), rook)
end

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
@deprecate sqrtm(A::UpperTriangular{T},::Type{Val{realmatrix}}) where {T,realmatrix} sqrtm(A, Val(realmatrix))
import .LinAlg: lufact, lufact!, qrfact, qrfact!, cholfact, cholfact!
@deprecate lufact(A::AbstractMatrix, ::Type{Val{false}}) lufact(A, Val(false))
@deprecate lufact(A::AbstractMatrix, ::Type{Val{true}}) lufact(A, Val(true))
@deprecate lufact!(A::AbstractMatrix, ::Type{Val{false}}) lufact!(A, Val(false))
@deprecate lufact!(A::AbstractMatrix, ::Type{Val{true}}) lufact!(A, Val(true))
@deprecate qrfact(A::AbstractMatrix, ::Type{Val{false}}) qrfact(A, Val(false))
@deprecate qrfact(A::AbstractMatrix, ::Type{Val{true}}) qrfact(A, Val(true))
@deprecate qrfact!(A::AbstractMatrix, ::Type{Val{false}}) qrfact!(A, Val(false))
@deprecate qrfact!(A::AbstractMatrix, ::Type{Val{true}}) qrfact!(A, Val(true))
@deprecate cholfact(A::AbstractMatrix, ::Type{Val{false}}) cholfact(A, Val(false))
@deprecate cholfact(A::AbstractMatrix, ::Type{Val{true}}; tol = 0.0) cholfact(A, Val(true); tol = tol)
@deprecate cholfact!(A::AbstractMatrix, ::Type{Val{false}}) cholfact!(A, Val(false))
@deprecate cholfact!(A::AbstractMatrix, ::Type{Val{true}}; tol = 0.0) cholfact!(A, Val(true); tol = tol)
@deprecate cat(::Type{Val{N}}, A::AbstractArray...) where {N} cat(Val(N), A...)
@deprecate cat_t(::Type{Val{N}}, ::Type{T}, A, B) where {N,T} cat_t(Val(N), T, A, B) false
@deprecate reshape(A::AbstractArray, ::Type{Val{N}}) where {N} reshape(A, Val(N))

@deprecate read(s::IO, x::Ref) read!(s, x)

@deprecate read(s::IO, t::Type, d1::Int, dims::Int...) read!(s, Array{t}(uninitialized, tuple(d1,dims...)))
@deprecate read(s::IO, t::Type, d1::Integer, dims::Integer...) read!(s, Array{t}(uninitialized, convert(Tuple{Vararg{Int}},tuple(d1,dims...))))
@deprecate read(s::IO, t::Type, dims::Dims) read!(s, Array{t}(uninitialized, dims))

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

# PR #22703
import .LinAlg: Bidiagonal
@deprecate Bidiagonal(dv::AbstractVector, ev::AbstractVector, isupper::Bool) Bidiagonal(dv, ev, ifelse(isupper, :U, :L))
@deprecate Bidiagonal(dv::AbstractVector, ev::AbstractVector, uplo::Char) Bidiagonal(dv, ev, ifelse(uplo == 'U', :U, :L))
@deprecate Bidiagonal(A::AbstractMatrix, isupper::Bool) Bidiagonal(A, ifelse(isupper, :U, :L))

@deprecate fieldnames(v) fieldnames(typeof(v))
# nfields(::Type) deprecation in builtins.c: update nfields tfunc in inference.jl when it is removed.
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
@deprecate eachline(cmd::AbstractCmd, stdin; chomp::Bool=true) eachline(pipeline(stdin, cmd), chomp=chomp)

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

# PR #22925
# also uncomment constructor tests in test/linalg/bidiag.jl
import .LinAlg: Bidiagonal
function Bidiagonal(dv::AbstractVector{T}, ev::AbstractVector{S}, uplo::Symbol) where {T,S}
    depwarn(string("`Bidiagonal(dv::AbstractVector{T}, ev::AbstractVector{S}, uplo::Symbol) where {T, S}`",
        " is deprecated, manually convert both vectors to the same type instead."), :Bidiagonal)
    R = promote_type(T, S)
    Bidiagonal(convert(Vector{R}, dv), convert(Vector{R}, ev), uplo)
end

# PR #23035
# also uncomment constructor tests in test/linalg/tridiag.jl
import .LinAlg: SymTridiagonal
function SymTridiagonal(dv::AbstractVector{T}, ev::AbstractVector{S}) where {T,S}
    depwarn(string("`SymTridiagonal(dv::AbstractVector{T}, ev::AbstractVector{S}) ",
        "where {T, S}` is deprecated, convert both vectors to the same type instead."), :SymTridiagonal)
    R = promote_type(T, S)
    SymTridiagonal(convert(Vector{R}, dv), convert(Vector{R}, ev))
end

# PR #23154
# also uncomment constructor tests in test/linalg/tridiag.jl
import .LinAlg: Tridiagonal
function Tridiagonal(dl::AbstractVector{Tl}, d::AbstractVector{Td}, du::AbstractVector{Tu}) where {Tl,Td,Tu}
    depwarn(string("`Tridiagonal(dl::AbstractVector{Tl}, d::AbstractVector{Td}, du::AbstractVector{Tu}) ",
        "where {Tl, Td, Tu}` is deprecated, convert all vectors to the same type instead."), :Tridiagonal)
    Tridiagonal(map(v->convert(Vector{promote_type(Tl,Td,Tu)}, v), (dl, d, du))...)
end

# deprecate sqrtm in favor of sqrt
@deprecate sqrtm sqrt

# deprecate expm in favor of exp
@deprecate expm! exp!
@deprecate expm exp

# deprecate logm in favor of log
@deprecate logm log

# PR #23092
@eval LibGit2 begin
    function prompt(msg::AbstractString; default::AbstractString="", password::Bool=false)
        Base.depwarn(string(
            "`LibGit2.prompt(msg::AbstractString; default::AbstractString=\"\", password::Bool=false)` is deprecated, use ",
            "`result = Base.prompt(msg, default=default, password=password); result === nothing ? \"\" : result` instead."), :prompt)
        coalesce(Base.prompt(msg, default=default, password=password), "")
    end
end

# PR #23187
@deprecate cpad(s, n::Integer, p=" ") rpad(lpad(s, div(n+textwidth(s), 2), p), n, p) false

# PR #22088
function hex2num(s::AbstractString)
    depwarn("`hex2num(s)` is deprecated. Use `reinterpret(Float64, parse(UInt64, s, 16))` instead.", :hex2num)
    if length(s) <= 4
        return reinterpret(Float16, parse(UInt16, s, 16))
    end
    if length(s) <= 8
        return reinterpret(Float32, parse(UInt32, s, 16))
    end
    return reinterpret(Float64, parse(UInt64, s, 16))
end
export hex2num

@deprecate num2hex(x::Union{Float16,Float32,Float64}) hex(reinterpret(Unsigned, x), sizeof(x)*2)
@deprecate num2hex(n::Integer) hex(n, sizeof(n)*2)

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

@deprecate convert(::Type{UInt128},     u::Random.UUID)     UInt128(u)
@deprecate convert(::Type{Random.UUID}, s::AbstractString)  Random.UUID(s)
@deprecate convert(::Type{Libc.FILE}, s::IO)  Libc.FILE(s)
@deprecate convert(::Type{VersionNumber}, v::Integer)         VersionNumber(v)
@deprecate convert(::Type{VersionNumber}, v::Tuple)           VersionNumber(v)
@deprecate convert(::Type{VersionNumber}, v::AbstractString)  VersionNumber(v)

@deprecate (convert(::Type{Integer}, x::Enum{T}) where {T<:Integer})         Integer(x)
@deprecate (convert(::Type{T}, x::Enum{T2}) where {T<:Integer,T2<:Integer})  T(x)

@deprecate convert(dt::Type{<:Integer}, ip::IPAddr)  dt(ip)

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
# related items to remove in: abstractarray.jl, dates/periods.jl, inference.jl
# also remove all uses of is_default_method

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

# PR #23373
import .LinAlg: diagm
@deprecate diagm(A::BitMatrix) BitMatrix(Diagonal(vec(A)))

# PR 23341
@eval GMP @deprecate gmp_version() version() false
@eval GMP @Base.deprecate_binding GMP_VERSION VERSION false
@eval GMP @deprecate gmp_bits_per_limb() bits_per_limb() false
@eval GMP @Base.deprecate_binding GMP_BITS_PER_LIMB BITS_PER_LIMB false
@eval MPFR @deprecate get_version() version() false
@eval LinAlg.LAPACK @deprecate laver() version() false

# PR #23427
@deprecate_binding e          ℯ true ", use ℯ (\\euler) or `Base.MathConstants.e`"
@deprecate_binding eu         ℯ true ", use ℯ (\\euler) or `Base.MathConstants.e`"
@deprecate_binding γ          MathConstants.γ
@deprecate_binding eulergamma MathConstants.eulergamma
@deprecate_binding catalan    MathConstants.catalan
@deprecate_binding φ          MathConstants.φ
@deprecate_binding golden     MathConstants.golden

# PR #23271
function IOContext(io::IO; kws...)
    depwarn("`IOContext(io, k=v, ...)` is deprecated, use `IOContext(io, :k => v, ...)` instead.", :IOContext)
    IOContext(io, (k=>v for (k, v) in pairs(kws))...)
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
@deprecate isleaftype isconcrete

# PR #22932
@deprecate +(a::Number, b::AbstractArray) broadcast(+, a, b)
@deprecate +(a::AbstractArray, b::Number) broadcast(+, a, b)
@deprecate -(a::Number, b::AbstractArray) broadcast(-, a, b)
@deprecate -(a::AbstractArray, b::Number) broadcast(-, a, b)

# PR #23640
# when this deprecation is deleted, remove all calls to it, and replace all keywords of:
# `payload::Union{CredentialPayload, AbstractCredential, CachedCredentials, Nothing}`
#  with `payload::CredentialPayload` from base/libgit2/libgit2.jl
@eval LibGit2 function deprecate_nullable_creds(f, sig, payload)
    if isa(payload, Union{AbstractCredential, CachedCredentials, Nothing})
        # Note: Be careful not to show the contents of the credentials as it could reveal a
        # password.
        if payload === nothing
            msg = "`LibGit2.$f($sig; payload=nothing)` is deprecated, use "
            msg *= "`LibGit2.$f($sig; payload=LibGit2.CredentialPayload())` instead."
            p = CredentialPayload()
        else
            cred = payload
            C = typeof(cred)
            msg = "`LibGit2.$f($sig; payload=$C(...))` is deprecated, use "
            msg *= "`LibGit2.$f($sig; payload=LibGit2.CredentialPayload($C(...)))` instead."
            p = CredentialPayload(cred)
        end
        Base.depwarn(msg, f)
    else
        p = payload::CredentialPayload
    end
    return p
end

# ease transition for return type change of e.g. indmax due to PR #22907 when used in the
# common pattern `ind2sub(size(a), indmax(a))`
@deprecate(ind2sub(dims::NTuple{N,Integer}, idx::CartesianIndex{N}) where N, Tuple(idx))

@deprecate contains(eq::Function, itr, x) any(y->eq(y,x), itr)

# deprecate zeros(D::Diagonal[, opts...])
function zeros(D::Diagonal)
    depwarn(string("`zeros(D::Diagonal)` is deprecated, use ",
        "`Diagonal(fill!(similar(D.diag), 0))` instead, or ",
        "`Diagonal(fill!(similar(D.diag), zero(eltype(D.diag))))` where necessary."), :zeros)
    return Diagonal(fill!(similar(D.diag), zero(eltype(D.diag))))
end
function zeros(D::Diagonal, ::Type{T}) where {T}
    depwarn(string("`zeros(D::Diagonal, ::Type{T}) where T` is deprecated, use ",
        "`Diagonal(fill!(similar(D.diag, T), 0))` instead, or ",
        "`Diagonal(fill!(similar(D.diag, T), zero(T)))` where necessary."), :zeros)
    return Diagonal(fill!(similar(D.diag, T), zero(T)))
end
function zeros(D::Diagonal, ::Type{T}, dims::Dims) where {T}
    depwarn(string("`zeros(D::Diagonal, ::Type{T}, dims::Dims) where T` is deprecated, ",
        "use `fill!(similar(D, T, dims), 0)` instead, or ",
        "`fill!(similar(D, T, dims), zero(T))` where necessary."), :zeros)
    return fill!(similar(D, T, dims), zero(T))
end
function zeros(D::Diagonal, ::Type{T}, dims::Integer...) where {T}
    depwarn(string("`zeros(D::Diagonal, ::Type{T}, dims::Integer...) where T` is deprecated, ",
        "use `fill!(similar(D, T, dims), 0)` instead, or ",
        "`fill!(similar(D, T, dims), zero(T))` where necessary."), :zeros)
    return fill!(similar(D, T, dims), zero(T))
end

# PR #23690
# `SSHCredential` and `UserPasswordCredential` constructors using `prompt_if_incorrect`
# are deprecated in base/libgit2/types.jl.

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

# PR #23711
@eval LibGit2 begin
    @deprecate get_creds!(cache::CachedCredentials, credid, default) get!(cache, credid, default)
end

## goodbeye, eye!
export eye
function eye(m::Integer)
    depwarn(string("`eye(m::Integer)` has been deprecated in favor of `I` and `Matrix` ",
        "constructors. For a direct replacement, consider `Matrix(1.0I, m, m)` or ",
        "`Matrix{Float64}(I, m, m)`. If `Float64` element type is not necessary, ",
        "consider the shorter `Matrix(I, m, m)` (with default `eltype(I)` `Bool`)."), :eye)
    return Matrix{Float64}(I, m, m)
end
function eye(::Type{T}, m::Integer) where T
    depwarn(string("`eye(T::Type, m::Integer)` has been deprecated in favor of `I` and ",
        "`Matrix` constructors. For a direct replacement, consider `Matrix{T}(I, m, m)`. If ",
        "`T` element type is not necessary, consider the shorter `Matrix(I, m, m)`",
        "(with default `eltype(I)` `Bool`)"), :eye)
    return Matrix{T}(I, m, m)
end
function eye(m::Integer, n::Integer)
    depwarn(string("`eye(m::Integer, n::Integer)` has been deprecated in favor of `I` and ",
        "`Matrix` constructors. For a direct replacement, consider `Matrix(1.0I, m, n)` ",
        "or `Matrix{Float64}(I, m, n)`. If `Float64` element type is not necessary, ",
        "consider the shorter `Matrix(I, m, n)` (with default `eltype(I)` `Bool`)."), :eye)
    return Matrix{Float64}(I, m, n)
end
function eye(::Type{T}, m::Integer, n::Integer) where T
    depwarn(string("`eye(T::Type, m::Integer, n::Integer)` has been deprecated in favor of ",
        "`I` and `Matrix` constructors. For a direct replacement, consider `Matrix{T}(I, m, n)`.",
        "If `T` element type is not necessary, consider the shorter `Matrix(I, m, n)` ",
        "(with default `eltype(I)` `Bool`)."), :eye)
    return Matrix{T}(I, m, n)
end
function eye(A::AbstractMatrix{T}) where T
    depwarn(string("`eye(A::AbstractMatrix{T}) where T` has been deprecated in favor of `I` and ",
        "`Matrix` constructors. For a direct replacement, consider `Matrix{eltype(A)}(I, size(A))`.",
        "If `eltype(A)` element type is not necessary, consider the shorter `Matrix(I, size(A))` ",
        "(with default `eltype(I)` `Bool`)."), :eye)
    return Matrix(one(T)I, size(A))
end
function eye(::Type{Diagonal{T}}, n::Int) where T
    depwarn(string("`eye(DT::Type{Diagonal{T}}, n::Int) where T` has been deprecated in favor of `I` ",
        "and `Diagonal` constructors. For a direct replacement, consider `Diagonal{T}(I, n)`. ",
        "If `T` element type is not necessary, consider the shorter `Diagonal(I, n)` ",
        "(with default `eltype(I)` `Bool`)."), :eye)
    return Diagonal{T}(I, n)
end
@eval Base.LinAlg import Base.eye


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

# PR #23816: deprecation of gradient
export gradient
@eval Base.LinAlg begin
    export gradient

    function gradient(args...)
        Base.depwarn("`gradient` is deprecated and will be removed in the next release.", :gradient)
        return _gradient(args...)
    end

    _gradient(F::BitVector) = _gradient(Array(F))
    _gradient(F::BitVector, h::Real) = _gradient(Array(F), h)
    _gradient(F::Vector, h::BitVector) = _gradient(F, Array(h))
    _gradient(F::BitVector, h::Vector) = _gradient(Array(F), h)
    _gradient(F::BitVector, h::BitVector) = _gradient(Array(F), Array(h))

    function _gradient(F::AbstractVector, h::Vector)
        n = length(F)
        T = typeof(oneunit(eltype(F))/oneunit(eltype(h)))
        g = similar(F, T)
        if n == 1
            g[1] = zero(T)
        elseif n > 1
            g[1] = (F[2] - F[1]) / (h[2] - h[1])
            g[n] = (F[n] - F[n-1]) / (h[end] - h[end-1])
            if n > 2
                h = h[3:n] - h[1:n-2]
                g[2:n-1] = (F[3:n] - F[1:n-2]) ./ h
            end
        end
        g
    end

    _gradient(F::AbstractVector) = _gradient(F, [1:length(F);])
    _gradient(F::AbstractVector, h::Real) = _gradient(F, [h*(1:length(F));])
end

@noinline function getaddrinfo(callback::Function, host::AbstractString)
    depwarn("`getaddrinfo` with a callback function is deprecated, wrap code in `@async` instead for deferred execution.", :getaddrinfo)
    @async begin
        r = getaddrinfo(host)
        callback(r)
    end
    nothing
end

@deprecate whos(io::IO, m::Module, pat::Regex) show(io, varinfo(m, pat))
@deprecate whos(io::IO, m::Module)             show(io, varinfo(m))
@deprecate whos(io::IO)                        show(io, varinfo())
@deprecate whos(m::Module, pat::Regex)         varinfo(m, pat)
@deprecate whos(m::Module)                     varinfo(m)
@deprecate whos(pat::Regex)                    varinfo(pat)
@deprecate whos()                              varinfo()

# indexing with A[true] will throw an argument error in the future
function to_index(i::Bool)
    depwarn("indexing with Bool values is deprecated. Convert the index to an integer first with `Int(i)`.", (:getindex, :setindex!, :view))
    convert(Int,i)::Int
end
# After deprecation is removed, enable the @testset "indexing by Bool values" in test/arrayops.jl
# Also un-comment the new definition in base/indices.jl

# deprecate odd fill! methods
@deprecate fill!(D::Diagonal, x)                       LinAlg.fillstored!(D, x)
@deprecate fill!(A::Base.LinAlg.AbstractTriangular, x) LinAlg.fillstored!(A, x)

# PR #25030
@eval LinAlg @deprecate fillslots! fillstored! false

function diagm(v::BitVector)
    depwarn(string("`diagm(v::BitVector)` is deprecated, use `diagm(0 => v)` or ",
        "`BitMatrix(Diagonal(v))` instead."), :diagm)
    return BitMatrix(Diagonal(v))
end
function diagm(v::AbstractVector)
    depwarn(string("`diagm(v::AbstractVector)` is deprecated, use `diagm(0 => v)` or ",
        "`Matrix(Diagonal(v))` instead."), :diagm)
    return Matrix(Diagonal(v))
end
@deprecate diagm(v::AbstractVector, k::Integer) diagm(k => v)
@deprecate diagm(x::Number) fill(x, 1, 1)

# deprecate BitArray{...}(shape...) constructors to BitArray{...}(uninitialized, shape...) equivalents
@deprecate BitArray{N}(dims::Vararg{Int,N}) where {N}   BitArray{N}(uninitialized, dims)
@deprecate BitArray(dims::NTuple{N,Int}) where {N}      BitArray(uninitialized, dims...)
@deprecate BitArray(dims::Integer...)                   BitArray(uninitialized, dims)

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

# full for structured arrays
function full(A::Union{Diagonal,Bidiagonal,Tridiagonal,SymTridiagonal})
    mattypestr = isa(A, Diagonal)        ? "Diagonal"        :
                 isa(A, Bidiagonal)      ? "Bidiagonal"      :
                 isa(A, Tridiagonal)     ? "Tridiagonal"     :
                 isa(A, SymTridiagonal)  ? "SymTridiagonal"  :
                    error("should not be reachable!")
    depwarn(string(
        "`full(A::$(mattypestr))` (and `full` in general) has been deprecated. ",
        "To replace `full(A::$(mattypestr))`, consider `Matrix(A)` or, if that ",
        "option is too narrow, `Array(A)`. Also consider `SparseMatrixCSC(A)` ",
        "or, if that option is too narrow, `sparse(A)`."),  :full)
    return Matrix(A)
end

# full for factorizations
function full(F::Union{LinAlg.LU,LinAlg.LQ,LinAlg.QR,LinAlg.QRPivoted,LinAlg.QRCompactWY,
                        LinAlg.SVD,LinAlg.LDLt,LinAlg.Schur,LinAlg.Eigen,LinAlg.Hessenberg,
                        LinAlg.Cholesky,LinAlg.CholeskyPivoted})
    facttypestr = isa(F, LinAlg.LU)               ? "LU"              :
                  isa(F, LinAlg.LQ)               ? "LQ"              :
                  isa(F, LinAlg.QR)               ? "QR"              :
                  isa(F, LinAlg.QRPivoted)        ? "QRPivoted"       :
                  isa(F, LinAlg.QRCompactWY)      ? "QRCompactWY"     :
                  isa(F, LinAlg.SVD)              ? "SVD"             :
                  isa(F, LinAlg.LDLt)             ? "LDLt"            :
                  isa(F, LinAlg.Schur)            ? "Schur"           :
                  isa(F, LinAlg.Eigen)            ? "Eigen"           :
                  isa(F, LinAlg.Hessenberg)       ? "Hessenberg"      :
                  isa(F, LinAlg.Cholesky)         ? "Cholesky"        :
                  isa(F, LinAlg.CholeskyPivoted)  ? "CholeskyPivoted" :
                      error("should not be reachable!")
   depwarn(string(
       "`full(F::$(facttypestr))` (and `full` in general) has been deprecated. ",
       "To replace `full(F::$(facttypestr))`, consider `Matrix(F)`, `AbstractMatrix(F)` or, ",
       "if those options are too narrow, `Array(F)` or `AbstractArray(F)`."), :full)
   return AbstractMatrix(F)
end

# full for implicit orthogonal factors
function full(Q::LinAlg.HessenbergQ)
    depwarn(string(
        "`full(Q::HessenbergQ)` (and `full` in general) has been deprecated. ",
        "To replace `full(Q::HessenbergQ)`, consider `Matrix(Q)` or, ",
        "if that option is too narrow, `Array(Q)`."), :full)
    return Matrix(Q)
end
function full(Q::LinAlg.LQPackedQ; thin::Bool = true)
    depwarn(string(
        "`full(Q::LQPackedQ; thin::Bool = true)` (and `full` in general) ",
        "has been deprecated. To replace `full(Q::LQPackedQ, true)`, ",
        "consider `Matrix(Q)` or `Array(Q)`. To replace `full(Q::LQPackedQ, false)`, ",
        "consider `Base.LinAlg.mul!(Q, Matrix{eltype(Q)}(I, size(Q.factors, 2), size(Q.factors, 2)))`."), :full)
    return thin ? Array(Q) : Base.LinAlg.mul!(Q, Matrix{eltype(Q)}(I, size(Q.factors, 2), size(Q.factors, 2)))
end
function full(Q::Union{LinAlg.QRPackedQ,LinAlg.QRCompactWYQ}; thin::Bool = true)
    qtypestr = isa(Q, LinAlg.QRPackedQ)    ? "QRPackedQ"    :
               isa(Q, LinAlg.QRCompactWYQ) ? "QRCompactWYQ" :
                  error("should not be reachable!")
    depwarn(string(
        "`full(Q::$(qtypestr); thin::Bool = true)` (and `full` in general) ",
        "has been deprecated. To replace `full(Q::$(qtypestr), true)`, ",
        "consider `Matrix(Q)` or `Array(Q)`. To replace `full(Q::$(qtypestr), false)`, ",
        "consider `Base.LinAlg.mul!(Q, Matrix{eltype(Q)}(I, size(Q.factors, 1), size(Q.factors, 1)))`."), :full)
    return thin ? Array(Q) : Base.LinAlg.mul!(Q, Matrix{eltype(Q)}(I, size(Q.factors, 1), size(Q.factors, 1)))
end

# full for symmetric / hermitian / triangular wrappers
function full(A::Symmetric)
    depwarn(string(
        "`full(A::Symmetric)` (and `full` in general) has been deprecated. ",
        "To replace `full(A::Symmetric)`, as appropriate consider `Matrix(A)`, ",
        "`Array(A)`, `SparseMatrixCSC(A)`, `sparse(A)`, `copyto!(similar(parent(A)), A)`, ",
        "or `Base.LinAlg.copytri!(copy(parent(A)), A.uplo)`."), :full)
    return Matrix(A)
end
function full(A::Hermitian)
    depwarn(string(
        "`full(A::Hermitian)` (and `full` in general) has been deprecated. ",
        "To replace `full(A::Hermitian)`, as appropriate consider `Matrix(A)`, ",
        "`Array(A)`, `SparseMatrixCSC(A)`, `sparse(A)`, `copyto!(similar(parent(A)), A)`, ",
        "or `Base.LinAlg.copytri!(copy(parent(A)), A.uplo, true)`."), :full)
    return Matrix(A)
end
function full(A::Union{UpperTriangular,LowerTriangular})
    (tritypestr, tri!str) =
        isa(A, UpperTriangular) ? ("UpperTriangular", "triu!") :
        isa(A, LowerTriangular) ? ("LowerTriangular", "tril!") :
            error("should not be reachable!")
    depwarn(string(
        "`full(A::$(tritypestr))` (and `full` in general) has been deprecated. ",
        "To replace `full(A::$(tritypestr))`, as appropriate consider `Matrix(A)`, ",
        "`Array(A)`, `SparseMatrixCSC(A)`, `sparse(A)`, `copyto!(similar(parent(A)), A)`, ",
        "or `$(tri!str)(copy(parent(A)))`."), :full)
    return Matrix(A)
end
function full(A::Union{LinAlg.UnitUpperTriangular,LinAlg.UnitLowerTriangular})
    tritypestr = isa(A, LinAlg.UnitUpperTriangular) ? "LinAlg.UnitUpperTriangular" :
                 isa(A, LinAlg.UnitLowerTriangular) ? "LinAlg.UnitLowerTriangular" :
                     error("should not be reachable!")
    depwarn(string(
        "`full(A::$(tritypestr))` (and `full` in general) has been deprecated. ",
        "To replace `full(A::$(tritypestr))`, as appropriate consider `Matrix(A)`, ",
        "`Array(A)`, `SparseMatrixCSC(A)`, `sparse(A)`, or `copyto!(similar(parent(A)), A)`."), :full)
    return Matrix(A)
end


# issue #20816
@deprecate strwidth textwidth
@deprecate charwidth textwidth

# TODO: after 0.7, remove thin keyword argument and associated logic from...
# (1) base/linalg/svd.jl
# (2) base/linalg/qr.jl
# (3) base/linalg/lq.jl

@deprecate find(x::Number)            find(!iszero, x)
@deprecate findnext(A, v, i::Integer) findnext(equalto(v), A, i)
@deprecate findfirst(A, v)            findfirst(equalto(v), A)
@deprecate findprev(A, v, i::Integer) findprev(equalto(v), A, i)
@deprecate findlast(A, v)             findlast(equalto(v), A)
# also remove deprecation warnings in find* functions in array.jl, sparse/sparsematrix.jl,
# and sparse/sparsevector.jl.

# issue #22849
@deprecate reinterpret(::Type{T}, a::Array{S}, dims::NTuple{N,Int}) where {T, S, N} reshape(reinterpret(T, vec(a)), dims)
@deprecate reinterpret(::Type{T}, a::ReinterpretArray{S}, dims::NTuple{N,Int}) where {T, S, N} reshape(reinterpret(T, vec(a)), dims)

# issue #24006
@deprecate linearindices(s::AbstractString) eachindex(s)

# deprecate Array(shape...)-like constructors to Array(uninitialized, shape...) equivalents
# --> former primitive constructors
@deprecate Array{T,1}(m::Int) where {T}                      Array{T,1}(uninitialized, m)
@deprecate Array{T,2}(m::Int, n::Int) where {T}              Array{T,2}(uninitialized, m, n)
@deprecate Array{T,3}(m::Int, n::Int, o::Int) where {T}      Array{T,3}(uninitialized, m, n, o)
@deprecate Array{T,N}(d::Vararg{Int,N}) where {T,N}          Array{T,N}(uninitialized, d)
@deprecate Array{T,N}(d::NTuple{N,Int}) where {T,N}          Array{T,N}(uninitialized, d)
@deprecate Array{T}(m::Int) where {T}                        Array{T}(uninitialized, m)
@deprecate Array{T}(m::Int, n::Int) where {T}                Array{T}(uninitialized, m, n)
@deprecate Array{T}(m::Int, n::Int, o::Int) where {T}        Array{T}(uninitialized, m, n, o)
@deprecate Array{T}(d::NTuple{N,Int}) where {T,N}            Array{T}(uninitialized, d)
# --> former convenience constructors
@deprecate Vector{T}(m::Integer) where {T}                          Vector{T}(uninitialized, m)
@deprecate Matrix{T}(m::Integer, n::Integer) where {T}              Matrix{T}(uninitialized, m, n)
@deprecate Array{T}(m::Integer) where {T}                           Array{T}(uninitialized, m)
@deprecate Array{T}(m::Integer, n::Integer) where {T}               Array{T}(uninitialized, m, n)
@deprecate Array{T}(m::Integer, n::Integer, o::Integer) where {T}   Array{T}(uninitialized, m, n, o)
@deprecate Array{T}(d::Integer...) where {T}                        Array{T}(uninitialized, d)
@deprecate Vector(m::Integer)                                       Vector(uninitialized, m)
@deprecate Matrix(m::Integer, n::Integer)                           Matrix(uninitialized, m, n)

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

@eval LinAlg begin
    @deprecate chol!(x::Number, uplo) chol(x) false
end

@deprecate cumsum(A::AbstractArray)     cumsum(A, 1)
@deprecate cumprod(A::AbstractArray)    cumprod(A, 1)
import .LinAlg: diff
@deprecate diff(A::AbstractMatrix) diff(A, 1)

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

# deprecate ConjArray
# TODO: between 0.7 and 1.0 remove
#       1) the type definitions in base/linalg/conjarray.jl
#       2) the include("base/linalg/conjarray.jl") from base/linalg/linalg.jl
#       3) the file base/linalg/conjarray.jl itself
@eval Base.LinAlg begin
    export ConjArray, ConjVector, ConjMatrix

    function ConjArray(a::AbstractArray{T,N}) where {T,N}
        Base.depwarn(_ConjArray_depstring(), :ConjArray)
        return ConjArray{conj_type(T),N,typeof(a)}(a)
    end
    function ConjVector(v::AbstractVector{T}) where {T}
        Base.depwarn(_ConjArray_depstring(), :ConjArray)
        return ConjArray{conj_type(T),1,typeof(v)}(v)
    end
    function ConjMatrix(m::AbstractMatrix{T}) where {T}
        Base.depwarn(_ConjArray_depstring(), :ConjArray)
        return ConjArray{conj_type(T),2,typeof(m)}(m)
    end

    _ConjArray_depstring() = string("`ConjRowVector` and `RowVector` have been deprecated in favor ",
            "of `Adjoint` and `Transpose`, and, as part of the implementation of `ConjRowVector`",
            "/`RowVector`, `ConjArray`s have been deprecated as well. Please see 0.7's NEWS.md ",
            "for a more detailed explanation of the associated changes.")

    # This type can cause the element type to change under conjugation - e.g. an array of complex arrays.
    @inline conj_type(x) = conj_type(typeof(x))
    @inline conj_type(::Type{T}) where {T} = promote_op(conj, T)

    @inline parent(c::ConjArray) = c.parent
    @inline parent_type(c::ConjArray) = parent_type(typeof(c))
    @inline parent_type(::Type{ConjArray{T,N,A}}) where {T,N,A} = A

    @inline size(a::ConjArray) = size(a.parent)
    IndexStyle(::CA) where {CA<:ConjArray} = IndexStyle(parent_type(CA))
    IndexStyle(::Type{CA}) where {CA<:ConjArray} = IndexStyle(parent_type(CA))

    @propagate_inbounds getindex(a::ConjArray{T,N}, i::Int) where {T,N} = conj(getindex(a.parent, i))
    @propagate_inbounds getindex(a::ConjArray{T,N}, i::Vararg{Int,N}) where {T,N} = conj(getindex(a.parent, i...))
    @propagate_inbounds setindex!(a::ConjArray{T,N}, v, i::Int) where {T,N} = setindex!(a.parent, conj(v), i)
    @propagate_inbounds setindex!(a::ConjArray{T,N}, v, i::Vararg{Int,N}) where {T,N} = setindex!(a.parent, conj(v), i...)

    @inline similar(a::ConjArray, ::Type{T}, dims::Dims{N}) where {T,N} = similar(parent(a), T, dims)

    # Currently, this is default behavior for RowVector only
    @inline conj(a::ConjArray) = parent(a)

    # Helper functions, currently used by RowVector
    @inline _conj(a::AbstractArray) = ConjArray(a)
    @inline _conj(a::AbstractArray{T}) where {T<:Real} = a
    @inline _conj(a::ConjArray) = parent(a)
    @inline _conj(a::ConjArray{T}) where {T<:Real} = parent(a)
end
@eval Base begin
    export ConjArray
end

# deprecate ConjRowVector/RowVector
# TODO: between 0.7 and 1.0 remove
#       1) the type definitions in base/linalg/rowvector.jl
#       2) the include("base/linalg/rowvector.jl") from base/linalg/linalg.jl
#       3) the file base/linalg/rowvector.jl itself
#       4) the RowVectors in the Unions in base/sparse/sparsevector.jl around lines 995, 1010, 1011, and 1012
@eval Base.LinAlg begin
    export RowVector

    _RowVector_depstring() = string("`ConjRowVector` and `RowVector` have been deprecated in favor ",
            "of `Adjoint` and `Transpose`. Please see 0.7's NEWS.md for a more detailed explanation ",
            "of the associated changes.")

    @inline check_types(::Type{T1}, ::AbstractVector{T2}) where {T1,T2} = check_types(T1, T2)
    @pure check_types(::Type{T1}, ::Type{T2}) where {T1,T2} = T1 === transpose_type(T2) ? nothing :
        error("Element type mismatch. Tried to create a `RowVector{$T1}` from an `AbstractVector{$T2}`")

    # The element type may be transformed as transpose is recursive
    @inline transpose_type(::Type{T}) where {T} = promote_op(transpose, T)

    # Constructors that take a vector
    function RowVector(vec::AbstractVector{T}) where {T}
        Base.depwarn(_RowVector_depstring(), :RowVector)
        return RowVector{transpose_type(T),typeof(vec)}(vec)
    end
    function RowVector{T}(vec::AbstractVector{T}) where {T}
        Base.depwarn(_RowVector_depstring(), :RowVector)
        return RowVector{T,typeof(vec)}(vec)
    end

    # Constructors that take a size and default to Array
    function RowVector{T}(::Uninitialized, n::Int) where {T}
        Base.depwarn(_RowVector_depstring(), :RowVector)
        return RowVector{T}(Vector{transpose_type(T)}(uninitialized, n))
    end
    function RowVector{T}(::Uninitialized, n1::Int, n2::Int) where {T}
        Base.depwarn(_RowVector_depstring(), :RowVector)
        return n1 == 1 ? RowVector{T}(Vector{transpose_type(T)}(uninitialized, n2)) :
            error("RowVector expects 1×N size, got ($n1,$n2)")
    end
    function RowVector{T}(::Uninitialized, n::Tuple{Int}) where {T}
        Base.depwarn(_RowVector_depstring(), :RowVector)
        return RowVector{T}(Vector{transpose_type(T)}(uninitialized, n[1]))
    end
    function RowVector{T}(::Uninitialized, n::Tuple{Int,Int}) where {T}
        Base.depwarn(_RowVector_depstring(), :RowVector)
        return n[1] == 1 ? RowVector{T}(Vector{transpose_type(T)}(uninitialized, n[2])) :
            error("RowVector expects 1×N size, got $n")
    end

    # Conversion of underlying storage
    convert(::Type{RowVector{T,V}}, rowvec::RowVector) where {T,V<:AbstractVector} =
        RowVector{T,V}(convert(V,rowvec.vec))

    # similar tries to maintain the RowVector wrapper and the parent type
    @inline similar(rowvec::RowVector) = RowVector(similar(parent(rowvec)))
    @inline similar(rowvec::RowVector, ::Type{T}) where {T} = RowVector(similar(parent(rowvec), transpose_type(T)))

    # Resizing similar currently loses its RowVector property.
    @inline similar(rowvec::RowVector, ::Type{T}, dims::Dims{N}) where {T,N} = similar(parent(rowvec), T, dims)

    # Basic methods

    # replaced in the Adjoint/Transpose transition
    # """
    #     transpose(v::AbstractVector)
    #
    # The transposition operator (`.'`).
    #
    # # Examples
    # ```jldoctest
    # julia> v = [1,2,3]
    # 3-element Array{Int64,1}:
    #  1
    #  2
    #  3
    #
    # julia> transpose(v)
    # 1×3 RowVector{Int64,Array{Int64,1}}:
    #  1  2  3
    # ```
    # """
    # @inline transpose(vec::AbstractVector) = RowVector(vec)
    # @inline adjoint(vec::AbstractVector) = RowVector(_conj(vec))

    # methods necessary to preserve RowVector's behavior through the Adjoint/Transpose transition
    rvadjoint(v::AbstractVector) = RowVector(_conj(v))
    rvtranspose(v::AbstractVector) = RowVector(v)
    rvadjoint(v::RowVector) = conj(v.vec)
    rvadjoint(v::RowVector{<:Real}) = v.vec
    rvtranspose(v::RowVector) = v.vec
    rvtranspose(v::ConjRowVector) = copy(v.vec)
    rvadjoint(x) = adjoint(x)
    rvtranspose(x) = transpose(x)

    @inline transpose(rowvec::RowVector) = rowvec.vec
    @inline transpose(rowvec::ConjRowVector) = copy(rowvec.vec) # remove the ConjArray wrapper from any raw vector
    @inline adjoint(rowvec::RowVector) = conj(rowvec.vec)
    @inline adjoint(rowvec::RowVector{<:Real}) = rowvec.vec

    parent(rowvec::RowVector) = rowvec.vec
    vec(rowvec::RowVector) = rowvec.vec

    """
        conj(v::RowVector)

    Return a [`ConjArray`](@ref) lazy view of the input, where each element is conjugated.

    # Examples
    ```jldoctest
    julia> v = RowVector([1+im, 1-im])
    1×2 RowVector{Complex{Int64},Array{Complex{Int64},1}}:
     1+1im  1-1im

    julia> conj(v)
    1×2 RowVector{Complex{Int64},ConjArray{Complex{Int64},1,Array{Complex{Int64},1}}}:
     1-1im  1+1im
    ```
    """
    @inline conj(rowvec::RowVector) = RowVector(_conj(rowvec.vec))
    @inline conj(rowvec::RowVector{<:Real}) = rowvec

    # AbstractArray interface
    @inline length(rowvec::RowVector) =  length(rowvec.vec)
    @inline size(rowvec::RowVector) = (1, length(rowvec.vec))
    @inline size(rowvec::RowVector, d) = ifelse(d==2, length(rowvec.vec), 1)
    @inline axes(rowvec::RowVector) = (Base.OneTo(1), axes(rowvec.vec)[1])
    @inline axes(rowvec::RowVector, d) = ifelse(d == 2, axes(rowvec.vec)[1], Base.OneTo(1))
    IndexStyle(::RowVector) = IndexLinear()
    IndexStyle(::Type{<:RowVector}) = IndexLinear()

    @propagate_inbounds getindex(rowvec::RowVector, i::Int) = rvtranspose(rowvec.vec[i])
    @propagate_inbounds setindex!(rowvec::RowVector, v, i::Int) = (setindex!(rowvec.vec, rvtranspose(v), i); rowvec)

    # Keep a RowVector where appropriate
    @propagate_inbounds getindex(rowvec::RowVector, ::Colon, i::Int) = rvtranspose.(rowvec.vec[i:i])
    @propagate_inbounds getindex(rowvec::RowVector, ::Colon, inds::AbstractArray{Int}) = RowVector(rowvec.vec[inds])
    @propagate_inbounds getindex(rowvec::RowVector, ::Colon, ::Colon) = RowVector(rowvec.vec[:])

    # helper function for below
    @inline to_vec(rowvec::RowVector) = map(rvtranspose, rvtranspose(rowvec))
    @inline to_vec(x::Number) = x
    @inline to_vecs(rowvecs...) = (map(to_vec, rowvecs)...,)

    # map: Preserve the RowVector by un-wrapping and re-wrapping, but note that `f`
    # expects to operate within the transposed domain, so to_vec transposes the elements
    @inline map(f, rowvecs::RowVector...) = RowVector(map(rvtranspose∘f, to_vecs(rowvecs...)...))

    # broacast (other combinations default to higher-dimensional array)
    @inline broadcast(f, rowvecs::Union{Number,RowVector}...) =
        RowVector(broadcast(transpose∘f, to_vecs(rowvecs...)...))

    # Horizontal concatenation #

    @inline hcat(X::RowVector...) = rvtranspose(vcat(map(rvtranspose, X)...))
    @inline hcat(X::Union{RowVector,Number}...) = rvtranspose(vcat(map(rvtranspose, X)...))

    @inline typed_hcat(::Type{T}, X::RowVector...) where {T} =
        rvtranspose(typed_vcat(T, map(rvtranspose, X)...))
    @inline typed_hcat(::Type{T}, X::Union{RowVector,Number}...) where {T} =
        rvtranspose(typed_vcat(T, map(rvtranspose, X)...))

    # Multiplication #

    # inner product -> dot product specializations
    @inline *(rowvec::RowVector{T}, vec::AbstractVector{T}) where {T<:Real} = dot(parent(rowvec), vec)
    @inline *(rowvec::ConjRowVector{T}, vec::AbstractVector{T}) where {T<:Real} = dot(rvadjoint(rowvec), vec)
    @inline *(rowvec::ConjRowVector, vec::AbstractVector) = dot(rvadjoint(rowvec), vec)

    # Generic behavior
    @inline function *(rowvec::RowVector, vec::AbstractVector)
        if length(rowvec) != length(vec)
            throw(DimensionMismatch("A has dimensions $(size(rowvec)) but B has dimensions $(size(vec))"))
        end
        sum(@inbounds(return rowvec[i]*vec[i]) for i = 1:length(vec))
    end
    @inline *(rowvec::RowVector, mat::AbstractMatrix) = rvtranspose(transpose(mat) * rvtranspose(rowvec))
    *(::RowVector, ::RowVector) = throw(DimensionMismatch("Cannot multiply two transposed vectors"))
    @inline *(vec::AbstractVector, rowvec::RowVector) = vec .* rowvec
    *(vec::AbstractVector, rowvec::AbstractVector) = throw(DimensionMismatch("Cannot multiply two vectors"))

    # Transposed forms
    *(::RowVector, ::Transpose{<:Any,<:AbstractVector}) =
        throw(DimensionMismatch("Cannot multiply two transposed vectors"))
    *(rowvec::RowVector, transmat::Transpose{<:Any,<:AbstractMatrix}) =
        (mat = transmat.parent; rvtranspose(mat * rvtranspose(rowvec)))
    *(rowvec1::RowVector, transrowvec2::Transpose{<:Any,<:RowVector}) =
        (rowvec2 = transrowvec2.parent; rowvec1*rvtranspose(rowvec2))
    *(::AbstractVector, ::Transpose{<:Any,<:RowVector}) =
        throw(DimensionMismatch("Cannot multiply two vectors"))
    *(mat::AbstractMatrix, transrowvec::Transpose{<:Any,<:RowVector}) =
        (rowvec = transrowvec.parent; mat * rvtranspose(rowvec))

    *(transrowvec::Transpose{<:Any,<:RowVector}, transvec::Transpose{<:Any,<:AbstractVector}) =
        rvtranspose(transrowvec.parent) * transpose(transvec.parent)
    *(transrowvec1::Transpose{<:Any,<:RowVector}, transrowvec2::Transpose{<:Any,<:RowVector}) =
        throw(DimensionMismatch("Cannot multiply two vectors"))
    *(transvec::Transpose{<:Any,<:AbstractVector}, transrowvec::Transpose{<:Any,<:RowVector}) =
        transpose(transvec.parent)*rvtranspose(transrowvec.parent)
    *(transmat::Transpose{<:Any,<:AbstractMatrix}, transrowvec::Transpose{<:Any,<:RowVector}) =
        transmat * rvtranspose(transrowvec.parent)

    *(::Transpose{<:Any,<:RowVector}, ::AbstractVector) =
        throw(DimensionMismatch("Cannot multiply two vectors"))
    *(transrowvec1::Transpose{<:Any,<:RowVector}, rowvec2::RowVector) =
        rvtranspose(transrowvec1.parent) * rowvec2
    *(transvec::Transpose{<:Any,<:AbstractVector}, rowvec::RowVector) =
        throw(DimensionMismatch("Cannot multiply two transposed vectors"))

    # Conjugated forms
    *(::RowVector, ::Adjoint{<:Any,<:AbstractVector}) =
        throw(DimensionMismatch("Cannot multiply two transposed vectors"))
    *(rowvec::RowVector, adjmat::Adjoint{<:Any,<:AbstractMatrix}) =
        rvadjoint(adjmat.parent * rvadjoint(rowvec))
    *(rowvec1::RowVector, adjrowvec2::Adjoint{<:Any,<:RowVector}) =
        rowvec1 * rvadjoint(adjrowvec2.parent)
    *(vec::AbstractVector, adjrowvec::Adjoint{<:Any,<:RowVector}) =
        throw(DimensionMismatch("Cannot multiply two vectors"))
    *(mat::AbstractMatrix, adjrowvec::Adjoint{<:Any,<:RowVector}) =
        mat * rvadjoint(adjrowvec.parent)

    *(adjrowvec::Adjoint{<:Any,<:RowVector}, adjvec::Adjoint{<:Any,<:AbstractVector}) =
        rvadjoint(adjrowvec.parent) * adjoint(adjvec.parent)
    *(adjrowvec1::Adjoint{<:Any,<:RowVector}, adjrowvec2::Adjoint{<:Any,<:RowVector}) =
        throw(DimensionMismatch("Cannot multiply two vectors"))
    *(adjvec::Adjoint{<:Any,<:AbstractVector}, adjrowvec::Adjoint{<:Any,<:RowVector}) =
        adjoint(adjvec.parent)*rvadjoint(adjrowvec.parent)
    *(adjmat::Adjoint{<:Any,<:AbstractMatrix}, adjrowvec::Adjoint{<:Any,<:RowVector}) =
        adjoint(adjmat.parent) * rvadjoint(adjrowvec.parent)

    *(::Adjoint{<:Any,<:RowVector}, ::AbstractVector) = throw(DimensionMismatch("Cannot multiply two vectors"))
    *(adjrowvec1::Adjoint{<:Any,<:RowVector}, rowvec2::RowVector) = rvadjoint(adjrowvec1.parent) * rowvec2
    *(adjvec::Adjoint{<:Any,<:AbstractVector}, rowvec::RowVector) = throw(DimensionMismatch("Cannot multiply two transposed vectors"))

    # Pseudo-inverse
    pinv(v::RowVector, tol::Real=0) = rvadjoint(pinv(rvadjoint(v), tol))

    # Left Division #

    \(rowvec1::RowVector, rowvec2::RowVector) = pinv(rowvec1) * rowvec2
    \(mat::AbstractMatrix, rowvec::RowVector) = throw(DimensionMismatch("Cannot left-divide transposed vector by matrix"))
    \(transmat::Transpose{<:Any,<:AbstractMatrix}, rowvec::RowVector) =
        throw(DimensionMismatch("Cannot left-divide transposed vector by matrix"))
    \(adjmat::Adjoint{<:Any,<:AbstractMatrix}, rowvec::RowVector) =
        throw(DimensionMismatch("Cannot left-divide transposed vector by matrix"))

    # Right Division #

    @inline /(rowvec::RowVector, mat::AbstractMatrix) = rvtranspose(transpose(mat) \ rvtranspose(rowvec))
    /(rowvec::RowVector, transmat::Transpose{<:Any,<:AbstractMatrix}) = rvtranspose(transmat.parent \ rvtranspose(rowvec))
    /(rowvec::RowVector, adjmat::Adjoint{<:Any,<:AbstractMatrix}) = rvadjoint(adjmat.parent \ rvadjoint(rowvec))


    # definitions necessary for test/linalg/dense.jl to pass
    # should be cleaned up / revised as necessary in the future
    /(A::Number, B::Adjoint{<:Any,<:RowVector}) = /(A, rvadjoint(B.parent))
    /(A::Matrix, B::RowVector) = rvadjoint(rvadjoint(B) \ adjoint(A))


    # dismabiguation methods
    *(A::Adjoint{<:Any,<:AbstractVector}, B::Transpose{<:Any,<:RowVector}) = adjoint(A.parent) * B
    *(A::Adjoint{<:Any,<:AbstractMatrix}, B::Transpose{<:Any,<:RowVector}) = A * rvtranspose(B.parent)
    *(A::Transpose{<:Any,<:AbstractVector}, B::Adjoint{<:Any,<:RowVector}) = transpose(A.parent) * B
    *(A::Transpose{<:Any,<:AbstractMatrix}, B::Adjoint{<:Any,<:RowVector}) = A * rvadjoint(B.parent)
end
@eval Base begin
    export RowVector
end
@eval Base.LinAlg begin
    # deprecate RowVector{T}(shape...) constructors to RowVector{T}(uninitialized, shape...) equivalents
    @deprecate RowVector{T}(n::Int) where {T}               RowVector{T}(uninitialized, n)
    @deprecate RowVector{T}(n1::Int, n2::Int) where {T}     RowVector{T}(uninitialized, n1, n2)
    @deprecate RowVector{T}(n::Tuple{Int}) where {T}        RowVector{T}(uninitialized, n)
    @deprecate RowVector{T}(n::Tuple{Int,Int}) where {T}    RowVector{T}(uninitialized, n)
end

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
# additionally, the following in-place ops were exported from Base
export A_mul_B!,
    A_mul_Bt!, At_mul_B!, At_mul_Bt!,
    A_mul_Bc!, Ac_mul_B!, Ac_mul_Bc!,
    A_ldiv_B!, At_ldiv_B!, Ac_ldiv_B!

# operations formerly exported from and imported/extended by Base.LinAlg
@eval Base.LinAlg begin
    import Base: A_mul_Bt, At_ldiv_Bt, A_rdiv_Bc, At_ldiv_B, Ac_mul_Bc, A_mul_Bc, Ac_mul_B,
        Ac_ldiv_B, Ac_ldiv_Bc, At_mul_Bt, A_rdiv_Bt, At_mul_B
    # most of these explicit exports are of course obviated by the deprecations below
    # but life is easier just leaving them for now...
    export A_ldiv_B!,
        A_ldiv_Bc,
        A_ldiv_Bt,
        A_mul_B!,
        A_mul_Bc,
        A_mul_Bc!,
        A_mul_Bt,
        A_mul_Bt!,
        A_rdiv_Bc,
        A_rdiv_Bt,
        Ac_ldiv_B,
        Ac_ldiv_Bc,
        Ac_ldiv_B!,
        Ac_mul_B,
        Ac_mul_B!,
        Ac_mul_Bc,
        Ac_mul_Bc!,
        Ac_rdiv_B,
        Ac_rdiv_Bc,
        At_ldiv_B,
        At_ldiv_Bt,
        At_ldiv_B!,
        At_mul_B,
        At_mul_B!,
        At_mul_Bt,
        At_mul_Bt!,
        At_rdiv_B,
        At_rdiv_Bt
end

# A[ct]_(mul|ldiv|rdiv)_B[ct][!] methods from base/linalg/bidiag.jl, to deprecate
@eval Base.LinAlg begin
    @deprecate A_mul_B!(C::AbstractMatrix, A::SymTridiagonal, B::BiTriSym)  mul!(C, A, B)
    @deprecate A_mul_B!(C::AbstractMatrix, A::BiTri, B::BiTriSym)           mul!(C, A, B)
    @deprecate A_mul_B!(C::AbstractMatrix, A::BiTriSym, B::BiTriSym)        mul!(C, A, B)
    @deprecate A_mul_B!(C::AbstractMatrix, A::AbstractTriangular, B::BiTriSym)  mul!(C, A, B)
    @deprecate A_mul_B!(C::AbstractMatrix, A::AbstractMatrix, B::BiTriSym)      mul!(C, A, B)
    @deprecate A_mul_B!(C::AbstractMatrix, A::Diagonal, B::BiTriSym)            mul!(C, A, B)
    @deprecate A_mul_B!(C::AbstractVector, A::BiTri, B::AbstractVector)         mul!(C, A, B)
    @deprecate A_mul_B!(C::AbstractMatrix, A::BiTri, B::AbstractVecOrMat)       mul!(C, A, B)
    @deprecate A_mul_B!(C::AbstractVecOrMat, A::BiTri, B::AbstractVecOrMat)     mul!(C, A, B)
    @deprecate Ac_ldiv_B(A::Bidiagonal, v::RowVector)   (\)(adjoint(A), v)
    @deprecate At_ldiv_B(A::Bidiagonal, v::RowVector)   (\)(transpose(A), v)
    @deprecate Ac_ldiv_B(A::Bidiagonal{<:Number}, v::RowVector{<:Number})   (\)(adjoint(A), v)
    @deprecate At_ldiv_B(A::Bidiagonal{<:Number}, v::RowVector{<:Number})   (\)(transpose(A), v)
    @deprecate Ac_mul_B(A::Bidiagonal{T}, B::AbstractVector{T}) where {T}   (*)(adjoint(A), B)
    @deprecate A_mul_Bc(A::Bidiagonal{T}, B::AbstractVector{T}) where {T}   (*)(A, adjoint(B))
    @deprecate A_rdiv_Bc(A::Bidiagonal{T}, B::AbstractVector{T}) where {T}  (/)(A, adjoint(B))
    @deprecate A_ldiv_B!(A::Union{Bidiagonal, AbstractTriangular}, b::AbstractVector)   ldiv!(A, b)
    @deprecate At_ldiv_B!(A::Bidiagonal, b::AbstractVector)     ldiv!(transpose(A), b)
    @deprecate Ac_ldiv_B!(A::Bidiagonal, b::AbstractVector)     ldiv!(adjoint(A), b)
    @deprecate A_ldiv_B!(A::Union{Bidiagonal,AbstractTriangular}, B::AbstractMatrix)    ldiv!(A, B)
    @deprecate Ac_ldiv_B!(A::Union{Bidiagonal,AbstractTriangular}, B::AbstractMatrix)   ldiv!(adjoint(A), B)
    @deprecate At_ldiv_B!(A::Union{Bidiagonal,AbstractTriangular}, B::AbstractMatrix)   ldiv!(transpose(A), B)
    @deprecate At_ldiv_B(A::Bidiagonal, B::AbstractVecOrMat)    (\)(transpose(A), B)
    @deprecate Ac_ldiv_B(A::Bidiagonal, B::AbstractVecOrMat)    ldiv!(adjoint(A), B)
    @deprecate Ac_ldiv_B(A::Bidiagonal{TA}, B::AbstractVecOrMat{TB}) where {TA<:Number,TB<:Number}  (\)(adjoint(A), B)
    @deprecate At_ldiv_B(A::Bidiagonal{TA}, B::AbstractVecOrMat{TB}) where {TA<:Number,TB<:Number}  (\)(transpose(A), B)
end

# A[ct]_(mul|ldiv|rdiv)_B[ct][!] methods from base/linalg/tridiag.jl, to deprecate
@eval Base.LinAlg begin
    @deprecate A_mul_B!(C::StridedVecOrMat, S::SymTridiagonal, B::StridedVecOrMat)  mul!(C, S, B)
end

# A[ct]_(mul|ldiv|rdiv)_B[ct][!] methods from base/linalg/diagonal.jl, to deprecate
@eval Base.LinAlg begin
    @deprecate A_mul_B!(A::Union{LowerTriangular,UpperTriangular}, D::Diagonal) mul!(A, D)
    @deprecate A_mul_B!(A::UnitLowerTriangular, D::Diagonal)    mul!(A, D)
    @deprecate A_mul_B!(A::UnitUpperTriangular, D::Diagonal)    mul!(A, D)
    @deprecate A_mul_B!(D::Diagonal, B::UnitLowerTriangular)    mul!(D, B)
    @deprecate A_mul_B!(D::Diagonal, B::UnitUpperTriangular)    mul!(D, B)
    @deprecate Ac_mul_B(D::Diagonal, B::Diagonal)           (*)(adjoint(D), B)
    @deprecate Ac_mul_B(A::AbstractTriangular, D::Diagonal) (*)(adjoint(A), D)
    @deprecate Ac_mul_B(A::AbstractMatrix, D::Diagonal)     (*)(adjoint(A), D)
    @deprecate At_mul_B(D::Diagonal, B::Diagonal)           (*)(transpose(D), B)
    @deprecate At_mul_B(A::AbstractTriangular, D::Diagonal) (*)(transpose(A), D)
    @deprecate At_mul_B(A::AbstractMatrix, D::Diagonal)     (*)(transpose(A), D)
    @deprecate A_mul_Bc(D::Diagonal, B::Diagonal)           (*)(D, adjoint(B))
    @deprecate A_mul_Bc(D::Diagonal, B::AbstractTriangular) (*)(D, adjoint(B))
    @deprecate A_mul_Bc(D::Diagonal, Q::Union{QRCompactWYQ,QRPackedQ})  (*)(D, adjoint(Q))
    @deprecate A_mul_Bc(D::Diagonal, A::AbstractMatrix)         (*)(D, adjoint(A))
    @deprecate A_mul_Bt(D::Diagonal, B::Diagonal)               (*)(D, transpose(B))
    @deprecate A_mul_Bt(D::Diagonal, B::AbstractTriangular)     (*)(D, transpose(B))
    @deprecate A_mul_Bt(D::Diagonal, A::AbstractMatrix) (*)(D, transpose(A))
    @deprecate Ac_mul_Bc(D::Diagonal, B::Diagonal)      (*)(adjoint(D), adjoint(B))
    @deprecate At_mul_Bt(D::Diagonal, B::Diagonal)      (*)(transpose(D), transpose(B))
    @deprecate A_mul_B!(A::Diagonal,B::Diagonal)        mul!(A, B)
    @deprecate At_mul_B!(A::Diagonal,B::Diagonal)       mul!(transpose(A), B)
    @deprecate Ac_mul_B!(A::Diagonal,B::Diagonal)       mul!(adjoint(A), B)
    @deprecate A_mul_B!(A::QRPackedQ, D::Diagonal)      mul!(A, D)
    @deprecate A_mul_B!(A::Diagonal,B::AbstractMatrix)      mul!(A, B)
    @deprecate At_mul_B!(A::Diagonal,B::AbstractMatrix)     mul!(transpose(A), B)
    @deprecate Ac_mul_B!(A::Diagonal,B::AbstractMatrix)     mul!(adjoint(A), B)
    @deprecate A_mul_B!(A::AbstractMatrix,B::Diagonal)      mul!(A, B)
    @deprecate A_mul_Bt!(A::AbstractMatrix,B::Diagonal)     mul!(A, transpose(B))
    @deprecate A_mul_Bc!(A::AbstractMatrix,B::Diagonal)     mul!(A, adjoint(B))
    @deprecate A_mul_B!(out::AbstractVector, A::Diagonal, in::AbstractVector)       mul!(out, A, in)
    @deprecate Ac_mul_B!(out::AbstractVector, A::Diagonal, in::AbstractVector)      mul!(out, adjoint(A), in)
    @deprecate At_mul_B!(out::AbstractVector, A::Diagonal, in::AbstractVector)      mul!(out, transpose(A), in)
    @deprecate A_mul_B!(out::AbstractMatrix, A::Diagonal, in::AbstractMatrix)       mul!(out, A, in)
    @deprecate Ac_mul_B!(out::AbstractMatrix, A::Diagonal, in::AbstractMatrix)      mul!(out, adjoint(A), in)
    @deprecate At_mul_B!(out::AbstractMatrix, A::Diagonal, in::AbstractMatrix)      mul!(out, transpose(A), in)
    @deprecate A_mul_Bt(A::Diagonal, B::RealHermSymComplexSym)      (*)(A, transpose(B))
    @deprecate At_mul_B(A::RealHermSymComplexSym, B::Diagonal)      (*)(transpose(A), B)
    @deprecate A_mul_Bc(A::Diagonal, B::RealHermSymComplexHerm)     (*)(A, adjoint(B))
    @deprecate Ac_mul_B(A::RealHermSymComplexHerm, B::Diagonal)     (*)(adjoint(A), B)
    @deprecate A_ldiv_B!(D::Diagonal{T}, v::AbstractVector{T}) where {T}        ldiv!(D, v)
    @deprecate A_ldiv_B!(D::Diagonal{T}, V::AbstractMatrix{T}) where {T}        ldiv!(D, V)
    @deprecate Ac_ldiv_B!(D::Diagonal{T}, B::AbstractVecOrMat{T}) where {T}     ldiv!(adjoint(D), B)
    @deprecate At_ldiv_B!(D::Diagonal{T}, B::AbstractVecOrMat{T}) where {T}     ldiv!(transpose(D), B)
    @deprecate A_rdiv_B!(A::AbstractMatrix{T}, D::Diagonal{T}) where {T}    rdiv!(A, D)
    @deprecate A_rdiv_Bc!(A::AbstractMatrix{T}, D::Diagonal{T}) where {T}   rdiv!(A, adjoint(D))
    @deprecate A_rdiv_Bt!(A::AbstractMatrix{T}, D::Diagonal{T}) where {T}   rdiv!(A, transpose(D))
    @deprecate Ac_ldiv_B(F::Factorization, D::Diagonal)     (\)(adjoint(F), D)
    @deprecate A_mul_Bt(D::Diagonal, rowvec::RowVector)     (*)(D, transpose(rowvec))
    @deprecate A_mul_Bc(D::Diagonal, rowvec::RowVector)     (*)(D, adjoint(rowvec))
    @deprecate A_ldiv_B!(D::Diagonal, B::StridedVecOrMat)   ldiv!(D, B)
end

# A[ct]_(mul|ldiv|rdiv)_B[ct][!] methods from base/linalg/special.jl, to deprecate
@eval Base.LinAlg begin
    @deprecate A_mul_Bc!(A::AbstractTriangular, B::Union{QRCompactWYQ,QRPackedQ})   mul!(A, adjoint(B))
    @deprecate A_mul_Bc(A::AbstractTriangular, B::Union{QRCompactWYQ,QRPackedQ})    (*)(A, adjoint(B))
end

# A[ct]_(mul|ldiv|rdiv)_B[ct][!] methods from base/linalg/bunchkaufman.jl, to deprecate
@eval Base.LinAlg begin
    @deprecate A_ldiv_B!(B::BunchKaufman{T}, R::StridedVecOrMat{T}) where {T<:BlasReal}     ldiv!(B, R)
    @deprecate A_ldiv_B!(B::BunchKaufman{T}, R::StridedVecOrMat{T}) where {T<:BlasComplex}  ldiv!(B, R)
    @deprecate A_ldiv_B!(B::BunchKaufman{T}, R::StridedVecOrMat{S}) where {T,S}             ldiv!(B, R)
end

# A[ct]_(mul|ldiv|rdiv)_B[ct][!] methods from base/linalg/cholesky.jl, to deprecate
@eval Base.LinAlg begin
    @deprecate A_ldiv_B!(C::Cholesky{T,<:AbstractMatrix}, B::StridedVecOrMat{T}) where {T<:BlasFloat}   ldiv!(C, B)
    @deprecate A_ldiv_B!(C::Cholesky{<:Any,<:AbstractMatrix}, B::StridedVecOrMat)           ldiv!(C, B)
    @deprecate A_ldiv_B!(C::CholeskyPivoted{T}, B::StridedVector{T}) where {T<:BlasFloat}   ldiv!(C, B)
    @deprecate A_ldiv_B!(C::CholeskyPivoted{T}, B::StridedMatrix{T}) where {T<:BlasFloat}   ldiv!(C, B)
    @deprecate A_ldiv_B!(C::CholeskyPivoted, B::StridedVector)      ldiv!(C, B)
    @deprecate A_ldiv_B!(C::CholeskyPivoted, B::StridedMatrix)      ldiv!(C, B)
end

# A[ct]_(mul|ldiv|rdiv)_B[ct][!] methods from base/linalg/factorization.jl, to deprecate
@eval Base.LinAlg begin
    @deprecate Ac_ldiv_B(F::Factorization, B::AbstractVecOrMat)     (\)(adjoint(F), B)
    @deprecate A_ldiv_B!(Y::AbstractVecOrMat, A::Factorization, B::AbstractVecOrMat)    ldiv!(Y, A, B)
    @deprecate Ac_ldiv_B!(Y::AbstractVecOrMat, A::Factorization, B::AbstractVecOrMat)   ldiv!(Y, adjoint(A), B)
    @deprecate At_ldiv_B!(Y::AbstractVecOrMat, A::Factorization, B::AbstractVecOrMat)   ldiv!(Y, transpose(A), B)
    @deprecate At_ldiv_B(F::Factorization{<:Real}, B::AbstractVecOrMat)     (\)(transpose(F), B)
    @deprecate At_ldiv_B(F::Factorization, B)   (\)(transpose(F), B)
end

# A[ct]_(mul|ldiv|rdiv)_B[ct][!] methods from base/linalg/hessenberg.jl, to deprecate
@eval Base.LinAlg begin
    @deprecate A_mul_B!(Q::HessenbergQ{T}, X::StridedVecOrMat{T}) where {T<:BlasFloat}  mul!(Q, X)
    @deprecate A_mul_B!(X::StridedMatrix{T}, Q::HessenbergQ{T}) where {T<:BlasFloat}    mul!(X, Q)
    @deprecate Ac_mul_B!(Q::HessenbergQ{T}, X::StridedVecOrMat{T}) where {T<:BlasFloat} mul!(adjoint(Q), X)
    @deprecate A_mul_Bc!(X::StridedMatrix{T}, Q::HessenbergQ{T}) where {T<:BlasFloat}   mul!(X, adjoint(Q))
    @deprecate Ac_mul_B(Q::HessenbergQ{T}, X::StridedVecOrMat{S}) where {T,S}   (*)(adjoint(Q), X)
    @deprecate A_mul_Bc(X::StridedVecOrMat{S}, Q::HessenbergQ{T}) where {T,S}   (*)(X, adjoint(Q))
end

# A[ct]_(mul|ldiv|rdiv)_B[ct][!] methods from base/linalg/ldlt.jl, to deprecate
@eval Base.LinAlg begin
    @deprecate A_ldiv_B!(S::LDLt{T,M}, B::AbstractVecOrMat{T}) where {T,M<:SymTridiagonal{T}}   ldiv!(S, B)
end

# A[ct]_(mul|ldiv|rdiv)_B[ct][!] methods from base/linalg/svd.jl, to deprecate
@eval Base.LinAlg begin
    @deprecate A_ldiv_B!(A::SVD{T}, B::StridedVecOrMat) where {T}   ldiv!(A, B)
end

# A[ct]_(mul|ldiv|rdiv)_B[ct][!] methods from base/linalg/symmetric.jl, to deprecate
@eval Base.LinAlg begin
    @deprecate A_mul_B!(y::StridedVector{T}, A::Symmetric{T,<:StridedMatrix}, x::StridedVector{T}) where {T<:BlasFloat}     mul!(y, A, x)
    @deprecate A_mul_B!(y::StridedVector{T}, A::Hermitian{T,<:StridedMatrix}, x::StridedVector{T}) where {T<:BlasReal}      mul!(y, A, x)
    @deprecate A_mul_B!(y::StridedVector{T}, A::Hermitian{T,<:StridedMatrix}, x::StridedVector{T}) where {T<:BlasComplex}   mul!(y, A, x)
    @deprecate A_mul_B!(C::StridedMatrix{T}, A::Symmetric{T,<:StridedMatrix}, B::StridedMatrix{T}) where {T<:BlasFloat} mul!(C, A, B)
    @deprecate A_mul_B!(C::StridedMatrix{T}, A::StridedMatrix{T}, B::Symmetric{T,<:StridedMatrix}) where {T<:BlasFloat} mul!(C, A, B)
    @deprecate A_mul_B!(C::StridedMatrix{T}, A::Hermitian{T,<:StridedMatrix}, B::StridedMatrix{T}) where {T<:BlasReal}  mul!(C, A, B)
    @deprecate A_mul_B!(C::StridedMatrix{T}, A::StridedMatrix{T}, B::Hermitian{T,<:StridedMatrix}) where {T<:BlasReal}  mul!(C, A, B)
    @deprecate A_mul_B!(C::StridedMatrix{T}, A::Hermitian{T,<:StridedMatrix}, B::StridedMatrix{T}) where {T<:BlasComplex}   mul!(C, A, B)
    @deprecate A_mul_B!(C::StridedMatrix{T}, A::StridedMatrix{T}, B::Hermitian{T,<:StridedMatrix}) where {T<:BlasComplex}   mul!(C, A, B)
    @deprecate At_mul_B(A::RealHermSymComplexSym, B::AbstractVector)    (*)(transpose(A), B)
    @deprecate At_mul_B(A::RealHermSymComplexSym, B::AbstractMatrix)    (*)(transpose(A), B)
    @deprecate A_mul_Bt(A::AbstractMatrix, B::RealHermSymComplexSym)    (*)(A, transpose(B))
    @deprecate Ac_mul_B(A::RealHermSymComplexHerm, B::AbstractVector)   (*)(adjoint(A), B)
    @deprecate Ac_mul_B(A::RealHermSymComplexHerm, B::AbstractMatrix)   (*)(adjoint(A), B)
    @deprecate A_mul_Bc(A::AbstractMatrix, B::RealHermSymComplexHerm)   (*)(A, adjoint(B))
    @deprecate A_mul_Bt(A::RowVector, B::RealHermSymComplexSym)     (*)(A, transpose(B))
    @deprecate A_mul_Bc(A::RowVector, B::RealHermSymComplexHerm)    (*)(A, adjoint(B))
    @deprecate At_mul_B(A::RealHermSymComplexSym, B::AbstractTriangular)    (*)(transpose(A), B)
    @deprecate A_mul_Bt(A::AbstractTriangular, B::RealHermSymComplexSym)    (*)(A, transpose(B))
    @deprecate Ac_mul_B(A::RealHermSymComplexHerm, B::AbstractTriangular)   (*)(adjoint(A), B)
    @deprecate A_mul_Bc(A::AbstractTriangular, B::RealHermSymComplexHerm)   (*)(A, adjoint(B))
end

# A[ct]_(mul|ldiv|rdiv)_B[ct][!] methods from base/linalg/lu.jl, to deprecate
@eval Base.LinAlg begin
    @deprecate A_ldiv_B!(A::LU{T,<:StridedMatrix}, B::StridedVecOrMat{T}) where {T<:BlasFloat}  ldiv!(A, B)
    @deprecate A_ldiv_B!(A::LU{<:Any,<:StridedMatrix}, B::StridedVecOrMat)  ldiv!(A, B)
    @deprecate At_ldiv_B!(A::LU{T,<:StridedMatrix}, B::StridedVecOrMat{T}) where {T<:BlasFloat}     ldiv!(transpose(A), B)
    @deprecate At_ldiv_B!(A::LU{<:Any,<:StridedMatrix}, B::StridedVecOrMat)     ldiv!(transpose(A), B)
    @deprecate Ac_ldiv_B!(F::LU{T,<:StridedMatrix}, B::StridedVecOrMat{T}) where {T<:Real}          ldiv!(adjoint(F), B)
    @deprecate Ac_ldiv_B!(A::LU{T,<:StridedMatrix}, B::StridedVecOrMat{T}) where {T<:BlasComplex}   ldiv!(adjoint(A), B)
    @deprecate Ac_ldiv_B!(A::LU{<:Any,<:StridedMatrix}, B::StridedVecOrMat)     ldiv!(adjoint(A), B)
    @deprecate At_ldiv_Bt(A::LU{T,<:StridedMatrix}, B::StridedVecOrMat{T}) where {T<:BlasFloat}  (\)(transpose(A), transpose(B))
    @deprecate At_ldiv_Bt(A::LU, B::StridedVecOrMat)    (\)(transpose(A), transpose(B))
    @deprecate Ac_ldiv_Bc(A::LU{T,<:StridedMatrix}, B::StridedVecOrMat{T}) where {T<:BlasComplex} (\)(adjoint(A), adjoint(B))
    @deprecate Ac_ldiv_Bc(A::LU, B::StridedVecOrMat)    (\)(adjoint(A), adjoint(B))
    @deprecate A_ldiv_B!(A::LU{T,Tridiagonal{T,V}}, B::AbstractVecOrMat) where {T,V}    ldiv!(A, B)
    @deprecate At_ldiv_B!(A::LU{T,Tridiagonal{T,V}}, B::AbstractVecOrMat) where {T,V}   (\)(transpose(A), B)
    @deprecate Ac_ldiv_B!(A::LU{T,Tridiagonal{T,V}}, B::AbstractVecOrMat) where {T,V}   ldiv!(adjoint(A), B)
end

# A[ct]_(mul|ldiv|rdiv)_B[ct][!] methods from base/linalg/lq.jl, to deprecate
@eval Base.LinAlg begin
    @deprecate A_mul_B!(A::LQ{T}, B::StridedVecOrMat{T}) where {T<:BlasFloat}   mul!(A, B)
    @deprecate A_mul_B!(A::LQ{T}, B::QR{T}) where {T<:BlasFloat}    mul!(A, B)
    @deprecate A_mul_B!(A::QR{T}, B::LQ{T}) where {T<:BlasFloat}    mul!(A, B)
    @deprecate A_mul_B!(A::LQPackedQ{T}, B::StridedVecOrMat{T}) where {T<:BlasFloat}    mul!(A, B)
    @deprecate Ac_mul_B!(A::LQPackedQ{T}, B::StridedVecOrMat{T}) where {T<:BlasReal}    mul!(adjoint(A), B)
    @deprecate Ac_mul_B!(A::LQPackedQ{T}, B::StridedVecOrMat{T}) where {T<:BlasComplex} mul!(adjoint(A), B)
    @deprecate Ac_mul_B(A::LQPackedQ, B::StridedVecOrMat)   (*)(adjoint(A), B)
    @deprecate A_mul_Bc(A::LQPackedQ, B::StridedVecOrMat)   (*)(A, adjoint(B))
    @deprecate Ac_mul_Bc(A::LQPackedQ, B::StridedVecOrMat)  (*)(adjoint(A), adjoint(B))
    @deprecate A_mul_B!(A::StridedMatrix{T}, B::LQPackedQ{T}) where {T<:BlasFloat}      mul!(A, B)
    @deprecate A_mul_Bc!(A::StridedMatrix{T}, B::LQPackedQ{T}) where {T<:BlasReal}      mul!(A, adjoint(B))
    @deprecate A_mul_Bc!(A::StridedMatrix{T}, B::LQPackedQ{T}) where {T<:BlasComplex}   mul!(A, adjoint(B))
    @deprecate A_mul_Bc(A::StridedVecOrMat, Q::LQPackedQ)   (*)(A, adjoint(Q))
    @deprecate Ac_mul_Bc(A::StridedMatrix, Q::LQPackedQ)    (*)(adjoint(A), adjoint(Q))
    @deprecate Ac_mul_B(A::StridedMatrix, Q::LQPackedQ)     (*)(adjoint(A), Q)
    @deprecate A_ldiv_B!(A::LQ{T}, B::StridedVecOrMat{T}) where {T} ldiv!(A, B)
end

# A[ct]_(mul|ldiv|rdiv)_B[ct][!] methods from base/linalg/qr.jl, to deprecate
@eval Base.LinAlg begin
    @deprecate A_mul_B!(A::QRCompactWYQ{T,S}, B::StridedVecOrMat{T}) where {T<:BlasFloat, S<:StridedMatrix} mul!(A, B)
    @deprecate A_mul_B!(A::QRPackedQ{T,S}, B::StridedVecOrMat{T}) where {T<:BlasFloat, S<:StridedMatrix}    mul!(A, B)
    @deprecate A_mul_B!(A::QRPackedQ, B::AbstractVecOrMat)  mul!(A, B)
    @deprecate Ac_mul_B!(A::QRCompactWYQ{T,S}, B::StridedVecOrMat{T}) where {T<:BlasReal,S<:StridedMatrix}      mul!(adjoint(A), B)
    @deprecate Ac_mul_B!(A::QRCompactWYQ{T,S}, B::StridedVecOrMat{T}) where {T<:BlasComplex,S<:StridedMatrix}   mul!(adjoint(A), B)
    @deprecate Ac_mul_B!(A::QRPackedQ{T,S}, B::StridedVecOrMat{T}) where {T<:BlasReal,S<:StridedMatrix}     mul!(adjoint(A), B)
    @deprecate Ac_mul_B!(A::QRPackedQ{T,S}, B::StridedVecOrMat{T}) where {T<:BlasComplex,S<:StridedMatrix}  mul!(adjoint(A), B)
    @deprecate Ac_mul_B!(A::QRPackedQ, B::AbstractVecOrMat) mul!(adjoint(A), B)
    @deprecate Ac_mul_B(Q::AbstractQ, B::StridedVecOrMat)   (*)(adjoint(Q), B)
    @deprecate A_mul_Bc(Q::AbstractQ, B::StridedVecOrMat)   (*)(Q, adjoint(B))
    @deprecate Ac_mul_Bc(Q::AbstractQ, B::StridedVecOrMat)  (*)(adjoint(Q), adjoint(B))
    @deprecate A_mul_B!(A::StridedVecOrMat{T}, B::QRCompactWYQ{T,S}) where {T<:BlasFloat,S<:StridedMatrix}  mul!(A, B)
    @deprecate A_mul_B!(A::StridedVecOrMat{T}, B::QRPackedQ{T,S}) where {T<:BlasFloat,S<:StridedMatrix}     mul!(A, B)
    @deprecate A_mul_B!(A::StridedMatrix,Q::QRPackedQ)  mul!(A, Q)
    @deprecate A_mul_Bc!(A::StridedVecOrMat{T}, B::QRCompactWYQ{T}) where {T<:BlasReal}     mul!(A, adjoint(B))
    @deprecate A_mul_Bc!(A::StridedVecOrMat{T}, B::QRCompactWYQ{T}) where {T<:BlasComplex}  mul!(A, adjoint(B))
    @deprecate A_mul_Bc!(A::StridedVecOrMat{T}, B::QRPackedQ{T}) where {T<:BlasReal}    mul!(A, adjoint(B))
    @deprecate A_mul_Bc!(A::StridedVecOrMat{T}, B::QRPackedQ{T}) where {T<:BlasComplex} mul!(A, adjoint(B))
    @deprecate A_mul_Bc!(A::StridedMatrix,Q::QRPackedQ)     mul!(A, adjoint(Q))
    @deprecate A_mul_Bc(A::StridedMatrix, B::AbstractQ)     (*)(A, adjoint(B))
    @deprecate A_mul_Bc(rowvec::RowVector, B::AbstractQ)    (*)(rowvec, adjoint(B))
    @deprecate Ac_mul_B(A::StridedVecOrMat, Q::AbstractQ)   (*)(adjoint(A), Q)
    @deprecate Ac_mul_Bc(A::StridedVecOrMat, Q::AbstractQ)  (*)(adjoint(A), adjoint(Q))
    @deprecate A_ldiv_B!(A::QRCompactWY{T}, b::StridedVector{T}) where {T<:BlasFloat}   ldiv!(A, b)
    @deprecate A_ldiv_B!(A::QRCompactWY{T}, B::StridedMatrix{T}) where {T<:BlasFloat}   ldiv!(A, B)
    @deprecate A_ldiv_B!(A::QRPivoted{T}, B::StridedMatrix{T}, rcond::Real) where {T<:BlasFloat}    ldiv!(A, B, rcond)
    @deprecate A_ldiv_B!(A::QRPivoted{T}, B::StridedVector{T}) where {T<:BlasFloat}     ldiv!(A, B)
    @deprecate A_ldiv_B!(A::QRPivoted{T}, B::StridedVecOrMat{T}) where {T<:BlasFloat}   ldiv!(A, B)
    @deprecate A_ldiv_B!(A::QR{T}, B::StridedMatrix{T}) where {T}   ldiv!(A, B)
    @deprecate A_ldiv_B!(A::QR, B::StridedVector)   ldiv!(A, B)
    @deprecate A_ldiv_B!(A::QRPivoted, b::StridedVector)    ldiv!(A, b)
    @deprecate A_ldiv_B!(A::QRPivoted, B::StridedMatrix)    ldiv!(A, B)
end

# A[ct]_(mul|ldiv|rdiv)_B[ct][!] methods from base/linalg/matmul.jl, to deprecate
@eval Base.LinAlg begin
    @deprecate Ac_mul_Bc(A::AbstractMatrix{T}, B::AbstractMatrix{S}) where {T,S}    (*)(adjoint(A), adjoint(B))
    @deprecate Ac_mul_Bc!(C::StridedMatrix{T}, A::StridedVecOrMat{T}, B::StridedVecOrMat{T}) where {T<:BlasFloat}   mul!(C, adjoint(A), adjoint(B))
    @deprecate Ac_mul_Bc!(C::AbstractMatrix, A::AbstractVecOrMat, B::AbstractVecOrMat)  mul!(C, adjoint(A), adjoint(B))
    @deprecate Ac_mul_Bt!(C::AbstractMatrix, A::AbstractVecOrMat, B::AbstractVecOrMat)  mul!(C, adjoint(A), transpose(B))
    @deprecate A_mul_Bc!(C::StridedMatrix{T}, A::StridedVecOrMat{T}, B::StridedVecOrMat{T}) where {T<:BlasComplex}  mul!(C, A, adjoint(B))
    @deprecate A_mul_Bc!(C::AbstractMatrix, A::AbstractVecOrMat, B::AbstractVecOrMat)   mul!(C, A, adjoint(B))
    @deprecate A_mul_Bc(A::AbstractMatrix{T}, B::AbstractMatrix{S}) where {T,S}     (*)(A, adjoint(B))
    @deprecate A_mul_Bc(A::StridedMatrix{<:BlasFloat}, B::StridedMatrix{<:BlasReal})    (*)(A, adjoint(B))
    @deprecate A_mul_Bc!(C::StridedMatrix{T}, A::StridedVecOrMat{T}, B::StridedVecOrMat{<:BlasReal}) where {T<:BlasFloat}   mul!(C, A, adjoint(B))
    @deprecate Ac_mul_B!(C::StridedMatrix{T}, A::StridedVecOrMat{T}, B::StridedVecOrMat{T}) where {T<:BlasComplex}  mul!(C, adjoint(A), B)
    @deprecate Ac_mul_B!(C::AbstractMatrix, A::AbstractVecOrMat, B::AbstractVecOrMat)   mul!(C, adjoint(A), B)
    @deprecate Ac_mul_B(A::AbstractMatrix{T}, B::AbstractMatrix{S}) where {T,S}         (*)(adjoint(A), B)
    @deprecate Ac_mul_B(A::StridedMatrix{T}, B::StridedMatrix{T}) where {T<:BlasReal}   (*)(adjoint(A), B)
    @deprecate Ac_mul_B!(C::StridedMatrix{T}, A::StridedVecOrMat{T}, B::StridedVecOrMat{T}) where {T<:BlasReal}     mul!(C, adjoint(A), B)
    @deprecate At_mul_Bt!(C::StridedMatrix{T}, A::StridedVecOrMat{T}, B::StridedVecOrMat{T}) where {T<:BlasFloat}   mul!(C, transpose(A), transpose(B))
    @deprecate At_mul_Bt!(C::AbstractMatrix, A::AbstractVecOrMat, B::AbstractVecOrMat)  mul!(C, transpose(A), transpose(B))
    @deprecate At_mul_Bt(A::AbstractMatrix{T}, B::AbstractVecOrMat{S}) where {T,S}      (*)(transpose(A), transpose(B))
    @deprecate A_mul_Bt!(C::AbstractVecOrMat, A::AbstractVecOrMat, B::AbstractVecOrMat) mul!(C, A, transpose(B))
    @deprecate A_mul_Bt!(C::StridedMatrix{Complex{Float32}}, A::StridedVecOrMat{Complex{Float32}}, B::StridedVecOrMat{Float32})     mul!(C, A, transpose(B))
    @deprecate A_mul_Bt!(C::StridedMatrix{Complex{Float64}}, A::StridedVecOrMat{Complex{Float64}}, B::StridedVecOrMat{Float64})     mul!(C, A, transpose(B))
    @deprecate A_mul_Bt!(C::StridedMatrix{T}, A::StridedVecOrMat{T}, B::StridedVecOrMat{T}) where {T<:BlasFloat}    mul!(C, A, transpose(B))
    @deprecate A_mul_Bt(A::AbstractMatrix{T}, B::AbstractMatrix{S}) where {T,S}     (*)(A, transpose(B))
    @deprecate At_mul_B!(C::StridedMatrix{T}, A::StridedVecOrMat{T}, B::StridedVecOrMat{T}) where {T<:BlasFloat}    mul!(C, transpose(A), B)
    @deprecate At_mul_B!(C::AbstractMatrix, A::AbstractVecOrMat, B::AbstractVecOrMat)   mul!(C, transpose(A), B)
    @deprecate At_mul_B(A::AbstractMatrix{T}, B::AbstractMatrix{S}) where {T,S}     (*)(transpose(A), B)
    @deprecate A_mul_B!(C::AbstractMatrix, A::AbstractVecOrMat, B::AbstractVecOrMat)    mul!(C, A, B)
    @deprecate A_mul_B!(C::StridedMatrix{Complex{Float32}}, A::StridedVecOrMat{Complex{Float32}}, B::StridedVecOrMat{Float32})  mul!(C, A, B)
    @deprecate A_mul_B!(C::StridedMatrix{Complex{Float64}}, A::StridedVecOrMat{Complex{Float64}}, B::StridedVecOrMat{Float64})  mul!(C, A, B)
    @deprecate A_mul_B!(C::StridedMatrix{T}, A::StridedVecOrMat{T}, B::StridedVecOrMat{T}) where {T<:BlasFloat}     mul!(C, A, B)
    @deprecate Ac_mul_B!(y::StridedVector{T}, A::StridedVecOrMat{T}, x::StridedVector{T}) where {T<:BlasReal}       mul!(y, adjoint(A), x)
    @deprecate Ac_mul_B!(y::StridedVector{T}, A::StridedVecOrMat{T}, x::StridedVector{T}) where {T<:BlasComplex}    mul!(y, adjoint(A), x)
    @deprecate Ac_mul_B!(y::AbstractVector, A::AbstractVecOrMat, x::AbstractVector)     mul!(y, adjoint(A), x)
    @deprecate Ac_mul_B(A::StridedMatrix{T}, x::StridedVector{S}) where {T<:BlasFloat,S}    (*)(adjoint(A), x)
    @deprecate Ac_mul_B(A::AbstractMatrix{T}, x::AbstractVector{S}) where {T,S}     (*)(adjoint(A), x)
    @deprecate At_mul_B(A::StridedMatrix{T}, x::StridedVector{S}) where {T<:BlasFloat,S}    (*)(transpose(A), x)
    @deprecate At_mul_B(A::AbstractMatrix{T}, x::AbstractVector{S}) where {T,S}     (*)(transpose(A), x)
    @deprecate At_mul_B!(y::StridedVector{T}, A::StridedVecOrMat{T}, x::StridedVector{T}) where {T<:BlasFloat}  mul!(y, transpose(A), x)
    @deprecate At_mul_B!(y::AbstractVector, A::AbstractVecOrMat, x::AbstractVector) mul!(y, transpose(A), x)
    @deprecate A_mul_B!(y::AbstractVector, A::AbstractVecOrMat, x::AbstractVector)  mul!(y, A, x)
    @deprecate A_mul_B!(y::StridedVector{Complex{Float32}}, A::StridedVecOrMat{Complex{Float32}}, x::StridedVector{Float32})    mul!(y, A, x)
    @deprecate A_mul_B!(y::StridedVector{Complex{Float64}}, A::StridedVecOrMat{Complex{Float64}}, x::StridedVector{Float64})    mul!(y, A, x)
    @deprecate A_mul_B!(y::StridedVector{T}, A::StridedVecOrMat{T}, x::StridedVector{T}) where {T<:BlasFloat}   mul!(y, A, x)
    @deprecate A_mul_Bt(a::AbstractVector, B::AbstractMatrix)   (*)(a, transpose(B))
    @deprecate A_mul_Bt(A::AbstractMatrix, b::AbstractVector)   (*)(A, transpose(b))
    @deprecate A_mul_Bc(a::AbstractVector, B::AbstractMatrix)   (*)(a, adjoint(B))
    @deprecate A_mul_Bc(A::AbstractMatrix, b::AbstractVector)   (*)(A, adjoint(b))
    @deprecate At_mul_B(x::StridedVector{T}, y::StridedVector{T}) where {T<:BlasComplex}    (*)(transpose(x), y)
end

# A[ct]_(mul|ldiv|rdiv)_B[ct][!] methods from base/linalg/triangular.jl, to deprecate
@eval Base.LinAlg begin
    @deprecate A_mul_Bc(A::AbstractTriangular, B::AbstractTriangular)   (*)(A, adjoint(B))
    @deprecate A_mul_Bt(A::AbstractTriangular, B::AbstractTriangular)   (*)(A, transpose(B))
    @deprecate Ac_mul_B(A::AbstractTriangular, B::AbstractTriangular)   (*)(adjoint(A), B)
    @deprecate At_mul_B(A::AbstractTriangular, B::AbstractTriangular)   (*)(transpose(A), B)
    @deprecate Ac_ldiv_B(A::Union{UpperTriangular,LowerTriangular}, B::RowVector)           (\)(adjoint(A), B)
    @deprecate Ac_ldiv_B(A::Union{UnitUpperTriangular,UnitLowerTriangular}, B::RowVector)   (\)(adjoint(A), B)
    @deprecate At_ldiv_B(A::Union{UpperTriangular,LowerTriangular}, B::RowVector)           (\)(transpose(A), B)
    @deprecate At_ldiv_B(A::Union{UnitUpperTriangular,UnitLowerTriangular}, B::RowVector)       (\)(transpose(A), B)
    @deprecate A_rdiv_Bc(rowvec::RowVector, A::Union{UpperTriangular,LowerTriangular})          (/)(rowvec, adjoint(A))
    @deprecate A_rdiv_Bc(rowvec::RowVector, A::Union{UnitUpperTriangular,UnitLowerTriangular})  (/)(rowvec, adjoint(A))
    @deprecate A_rdiv_Bt(rowvec::RowVector, A::Union{UpperTriangular,LowerTriangular})          (/)(rowvec, transpose(A))
    @deprecate A_rdiv_Bt(rowvec::RowVector, A::Union{UnitUpperTriangular,UnitLowerTriangular})  (/)(rowvec, transpose(A))
    @deprecate A_mul_Bt(rowvec::RowVector, A::AbstractTriangular)       (*)(rowvec, transpose(A))
    @deprecate A_mul_Bt(A::AbstractTriangular, rowvec::RowVector)       (*)(A, transpose(rowvec))
    @deprecate At_mul_Bt(A::AbstractTriangular, rowvec::RowVector)      (*)(transpose(A), transpose(rowvec))
    @deprecate A_mul_Bc(rowvec::RowVector, A::AbstractTriangular)       (*)(rowvec, adjoint(A))
    @deprecate A_mul_Bc(A::AbstractTriangular, rowvec::RowVector)       (*)(A, adjoint(rowvec))
    @deprecate Ac_mul_Bc(A::AbstractTriangular, rowvec::RowVector)      (*)(adjoint(A), adjoint(rowvec))
    @deprecate Ac_mul_B(A::AbstractMatrix, B::AbstractTriangular)       (*)(adjoint(A), B)
    @deprecate At_mul_B(A::AbstractMatrix, B::AbstractTriangular)       (*)(transpose(A), B)
    @deprecate A_mul_Bc(A::AbstractTriangular, B::AbstractMatrix)       (*)(A, adjoint(B))
    @deprecate A_mul_Bt(A::AbstractTriangular, B::AbstractMatrix)       (*)(A, transpose(B))
    @deprecate Ac_mul_Bc(A::AbstractTriangular, B::AbstractTriangular)  (*)(adjoint(A), adjoint(B))
    @deprecate Ac_mul_Bc(A::AbstractTriangular, B::AbstractMatrix)      (*)(adjoint(A), adjoint(B))
    @deprecate Ac_mul_Bc(A::AbstractMatrix, B::AbstractTriangular)      (*)(adjoint(A), adjoint(B))
    @deprecate At_mul_Bt(A::AbstractTriangular, B::AbstractTriangular)  (*)(transpose(A), transpose(B))
    @deprecate At_mul_Bt(A::AbstractTriangular, B::AbstractMatrix)      (*)(transpose(A), transpose(B))
    @deprecate At_mul_Bt(A::AbstractMatrix, B::AbstractTriangular)      (*)(transpose(A), transpose(B))
    @deprecate A_mul_Bc!(A::UpperTriangular, B::Union{LowerTriangular,UnitLowerTriangular})     mul!(A, adjoint(B))
    @deprecate A_mul_Bc!(A::LowerTriangular, B::Union{UpperTriangular,UnitUpperTriangular})     mul!(A, adjoint(B))
    @deprecate A_mul_Bt!(A::UpperTriangular, B::Union{LowerTriangular,UnitLowerTriangular})     mul!(A, transpose(B))
    @deprecate A_mul_Bt!(A::LowerTriangular, B::Union{UpperTriangular,UnitUpperTriangular})     mul!(A, transpose(B))
    @deprecate A_rdiv_Bc!(A::UpperTriangular, B::Union{LowerTriangular,UnitLowerTriangular})    rdiv!(A, adjoint(B))
    @deprecate A_rdiv_Bc!(A::LowerTriangular, B::Union{UpperTriangular,UnitUpperTriangular})    rdiv!(A, adjoint(B))
    @deprecate A_rdiv_Bt!(A::UpperTriangular, B::Union{LowerTriangular,UnitLowerTriangular})    rdiv!(A, transpose(B))
    @deprecate A_rdiv_Bt!(A::LowerTriangular, B::Union{UpperTriangular,UnitUpperTriangular})    rdiv!(A, transpose(B))
    @deprecate A_rdiv_B!(A::UpperTriangular, B::Union{UpperTriangular,UnitUpperTriangular})     rdiv!(A, B)
    @deprecate A_rdiv_B!(A::LowerTriangular, B::Union{LowerTriangular,UnitLowerTriangular})     rdiv!(A, B)
    @deprecate Ac_mul_B!(A::Union{LowerTriangular,UnitLowerTriangular}, B::UpperTriangular)     mul!(adjoint(A), B)
    @deprecate Ac_mul_B!(A::Union{UpperTriangular,UnitUpperTriangular}, B::LowerTriangular)     mul!(adjoint(A), B)
    @deprecate At_mul_B!(A::Union{LowerTriangular,UnitLowerTriangular}, B::UpperTriangular)     mul!(transpose(A), B)
    @deprecate At_mul_B!(A::Union{UpperTriangular,UnitUpperTriangular}, B::LowerTriangular)     mul!(transpose(A), B)
    @deprecate Ac_ldiv_B!(A::Union{LowerTriangular,UnitLowerTriangular}, B::UpperTriangular)    ldiv!(adjoint(A), B)
    @deprecate Ac_ldiv_B!(A::Union{UpperTriangular,UnitUpperTriangular}, B::LowerTriangular)    ldiv!(adjoint(A), B)
    @deprecate At_ldiv_B!(A::Union{LowerTriangular,UnitLowerTriangular}, B::UpperTriangular)    ldiv!(transpose(A), B)
    @deprecate At_ldiv_B!(A::Union{UpperTriangular,UnitUpperTriangular}, B::LowerTriangular)    ldiv!(transpose(A), B)
    @deprecate A_rdiv_Bt!(A::StridedMatrix, B::UnitLowerTriangular) rdiv!(A, transpose(B))
    @deprecate A_rdiv_Bt!(A::StridedMatrix, B::LowerTriangular)     rdiv!(A, transpose(B))
    @deprecate A_rdiv_Bt!(A::StridedMatrix, B::UnitUpperTriangular) rdiv!(A, transpose(B))
    @deprecate A_rdiv_Bt!(A::StridedMatrix, B::UpperTriangular)     rdiv!(A, transpose(B))
    @deprecate A_rdiv_Bc!(A::StridedMatrix, B::UnitLowerTriangular) rdiv!(A, adjoint(B))
    @deprecate A_rdiv_Bc!(A::StridedMatrix, B::LowerTriangular)     rdiv!(A, adjoint(B))
    @deprecate A_rdiv_Bc!(A::StridedMatrix, B::UnitUpperTriangular) rdiv!(A, adjoint(B))
    @deprecate A_rdiv_Bc!(A::StridedMatrix, B::UpperTriangular)     rdiv!(A, adjoint(B))
    @deprecate A_rdiv_B!(A::StridedMatrix, B::UnitLowerTriangular)  rdiv!(A, B)
    @deprecate A_rdiv_B!(A::StridedMatrix, B::LowerTriangular)      rdiv!(A, B)
    @deprecate A_rdiv_B!(A::StridedMatrix, B::UnitUpperTriangular)  rdiv!(A, B)
    @deprecate A_rdiv_B!(A::StridedMatrix, B::UpperTriangular)      rdiv!(A, B)
    @deprecate Ac_ldiv_B!(A::UnitUpperTriangular, b::AbstractVector, x::AbstractVector = b) ldiv!(adjoint(A), b, x)
    @deprecate Ac_ldiv_B!(A::UpperTriangular, b::AbstractVector, x::AbstractVector = b)     ldiv!(adjoint(A), b, x)
    @deprecate Ac_ldiv_B!(A::UnitLowerTriangular, b::AbstractVector, x::AbstractVector = b) ldiv!(adjoint(A), b, x)
    @deprecate Ac_ldiv_B!(A::LowerTriangular, b::AbstractVector, x::AbstractVector = b)     ldiv!(adjoint(A), b, x)
    @deprecate At_ldiv_B!(A::UnitUpperTriangular, b::AbstractVector, x::AbstractVector = b) ldiv!(transpose(A), b, x)
    @deprecate At_ldiv_B!(A::UpperTriangular, b::AbstractVector, x::AbstractVector = b)     ldiv!(transpose(A), b, x)
    @deprecate At_ldiv_B!(A::UnitLowerTriangular, b::AbstractVector, x::AbstractVector = b) ldiv!(transpose(A), b, x)
    @deprecate At_ldiv_B!(A::LowerTriangular, b::AbstractVector, x::AbstractVector = b)     ldiv!(transpose(A), b, x)
    @deprecate A_mul_Bt!(A::StridedMatrix, B::UnitLowerTriangular)  mul!(A, transpose(B))
    @deprecate A_mul_Bt!(A::StridedMatrix, B::LowerTriangular)      mul!(A, transpose(B))
    @deprecate A_mul_Bt!(A::StridedMatrix, B::UnitUpperTriangular)  mul!(A, transpose(B))
    @deprecate A_mul_Bt!(A::StridedMatrix, B::UpperTriangular)      mul!(A, transpose(B))
    @deprecate A_mul_Bc!(A::StridedMatrix, B::UnitLowerTriangular)  mul!(A, adjoint(B))
    @deprecate A_mul_Bc!(A::StridedMatrix, B::LowerTriangular)      mul!(A, adjoint(B))
    @deprecate A_mul_Bc!(A::StridedMatrix, B::UnitUpperTriangular)  mul!(A, adjoint(B))
    @deprecate A_mul_Bc!(A::StridedMatrix, B::UpperTriangular)      mul!(A, adjoint(B))
    @deprecate A_mul_B!(A::StridedMatrix, B::UnitLowerTriangular)   mul!(A, B)
    @deprecate A_mul_B!(A::StridedMatrix, B::LowerTriangular)       mul!(A, B)
    @deprecate A_mul_B!(A::StridedMatrix, B::UnitUpperTriangular)   mul!(A, B)
    @deprecate A_mul_B!(A::StridedMatrix, B::UpperTriangular)       mul!(A, B)
    @deprecate At_mul_B!(A::UnitLowerTriangular, B::StridedVecOrMat)    mul!(transpose(A), B)
    @deprecate At_mul_B!(A::LowerTriangular, B::StridedVecOrMat)        mul!(transpose(A), B)
    @deprecate At_mul_B!(A::UnitUpperTriangular, B::StridedVecOrMat)    mul!(transpose(A), B)
    @deprecate At_mul_B!(A::UpperTriangular, B::StridedVecOrMat)        mul!(transpose(A), B)
    @deprecate Ac_mul_B!(A::UnitLowerTriangular, B::StridedVecOrMat)    mul!(adjoint(A), B)
    @deprecate Ac_mul_B!(A::LowerTriangular, B::StridedVecOrMat)        mul!(adjoint(A), B)
    @deprecate Ac_mul_B!(A::UnitUpperTriangular, B::StridedVecOrMat)    mul!(adjoint(A), B)
    @deprecate Ac_mul_B!(A::UpperTriangular, B::StridedVecOrMat)    mul!(adjoint(A), B)
    @deprecate A_mul_B!(A::UnitLowerTriangular, B::StridedVecOrMat) mul!(A, B)
    @deprecate A_mul_B!(A::LowerTriangular, B::StridedVecOrMat)     mul!(A, B)
    @deprecate A_mul_B!(A::UnitUpperTriangular, B::StridedVecOrMat) mul!(A, B)
    @deprecate A_mul_B!(A::UpperTriangular, B::StridedVecOrMat)     mul!(A, B)
    @deprecate A_mul_B!(C::AbstractVector  , A::AbstractTriangular, B::AbstractVector)      mul!(C, A, B)
    @deprecate A_mul_B!(C::AbstractMatrix  , A::AbstractTriangular, B::AbstractVecOrMat)    mul!(C, A, B)
    @deprecate A_mul_B!(C::AbstractVecOrMat, A::AbstractTriangular, B::AbstractVecOrMat)    mul!(C, A, B)
    @deprecate Ac_mul_B!(C::AbstractVector  , A::AbstractTriangular, B::AbstractVector)     mul!(C, adjoint(A), B)
    @deprecate Ac_mul_B!(C::AbstractMatrix  , A::AbstractTriangular, B::AbstractVecOrMat)   mul!(C, adjoint(A), B)
    @deprecate Ac_mul_B!(C::AbstractVecOrMat, A::AbstractTriangular, B::AbstractVecOrMat)   mul!(C, adjoint(A), B)
    @deprecate At_mul_B!(C::AbstractVector  , A::AbstractTriangular, B::AbstractVector)     mul!(C, transpose(A), B)
    @deprecate At_mul_B!(C::AbstractMatrix  , A::AbstractTriangular, B::AbstractVecOrMat)   mul!(C, transpose(A), B)
    @deprecate At_mul_B!(C::AbstractVecOrMat, A::AbstractTriangular, B::AbstractVecOrMat)   mul!(C, transpose(A), B)
    @deprecate A_mul_B!(A::Tridiagonal, B::AbstractTriangular)  mul!(A, B)
    @deprecate A_mul_B!(C::AbstractMatrix, A::AbstractTriangular, B::Tridiagonal)   mul!(C, A, B)
    @deprecate A_mul_B!(C::AbstractMatrix, A::Tridiagonal, B::AbstractTriangular)   mul!(C, A, B)
    @deprecate A_mul_Bt!(C::AbstractVecOrMat, A::AbstractTriangular, B::AbstractVecOrMat)   mul!(C, A, transpose(B))
    @deprecate A_mul_Bc!(C::AbstractMatrix, A::AbstractTriangular, B::AbstractVecOrMat)     mul!(C, A, adjoint(B))
    @deprecate A_mul_Bc!(C::AbstractVecOrMat, A::AbstractTriangular, B::AbstractVecOrMat)   mul!(C, A, adjoint(B))
end
for mat in (:AbstractVector, :AbstractMatrix)
    @eval Base.LinAlg begin
        @deprecate Ac_mul_B(A::AbstractTriangular, B::$mat)     (*)(adjoint(A), B)
        @deprecate At_mul_B(A::AbstractTriangular, B::$mat)     (*)(transpose(A), B)
        @deprecate Ac_ldiv_B(A::Union{UnitUpperTriangular,UnitLowerTriangular}, B::$mat)    (\)(adjoint(A), B)
        @deprecate At_ldiv_B(A::Union{UnitUpperTriangular,UnitLowerTriangular}, B::$mat)    (\)(transpose(A), B)
        @deprecate Ac_ldiv_B(A::Union{UpperTriangular,LowerTriangular}, B::$mat)    (\)(adjoint(A), B)
        @deprecate At_ldiv_B(A::Union{UpperTriangular,LowerTriangular}, B::$mat)    (\)(transpose(A), B)
        @deprecate A_rdiv_Bc(A::$mat, B::Union{UnitUpperTriangular, UnitLowerTriangular})   (/)(A, adjoint(B))
        @deprecate A_rdiv_Bt(A::$mat, B::Union{UnitUpperTriangular, UnitLowerTriangular})   (/)(A, transpose(B))
        @deprecate A_rdiv_Bc(A::$mat, B::Union{UpperTriangular,LowerTriangular})    (/)(A, adjoint(B))
        @deprecate A_rdiv_Bt(A::$mat, B::Union{UpperTriangular,LowerTriangular})    (/)(A, transpose(B))
    end
end
@eval Base.LinAlg begin
    @deprecate A_mul_Bc(A::AbstractMatrix, B::AbstractTriangular)  (*)(A, adjoint(B))
    @deprecate A_mul_Bt(A::AbstractMatrix, B::AbstractTriangular)  (*)(A, transpose(B))
end
for (f, op, transform) in (
        (:A_mul_Bc, :*, :adjoint),
        (:A_mul_Bt, :*, :transpose),
        (:A_rdiv_Bc, :/, :adjoint),
        (:A_rdiv_Bt, :/, :transpose))
    @eval Base.LinAlg begin
        @deprecate $f(A::LowerTriangular, B::UpperTriangular)       ($op)(A, ($transform)(B))
        @deprecate $f(A::LowerTriangular, B::UnitUpperTriangular)   ($op)(A, ($transform)(B))
        @deprecate $f(A::UpperTriangular, B::LowerTriangular)       ($op)(A, ($transform)(B))
        @deprecate $f(A::UpperTriangular, B::UnitLowerTriangular)   ($op)(A, ($transform)(B))
    end
end
for (f, op, transform) in (
        (:Ac_mul_B, :*, :adjoint),
        (:At_mul_B, :*, :transpose),
        (:Ac_ldiv_B, :\, :adjoint),
        (:At_ldiv_B, :\, :transpose))
    @eval Base.LinAlg begin
        @deprecate ($f)(A::UpperTriangular, B::LowerTriangular)     ($op)(($transform)(A), B)
        @deprecate ($f)(A::UnitUpperTriangular, B::LowerTriangular) ($op)(($transform)(A), B)
        @deprecate ($f)(A::LowerTriangular, B::UpperTriangular)     ($op)(($transform)(A), B)
        @deprecate ($f)(A::UnitLowerTriangular, B::UpperTriangular) ($op)(($transform)(A), B)
    end
end
for (t, uploc, isunitc) in ((:LowerTriangular, 'L', 'N'),
                            (:UnitLowerTriangular, 'L', 'U'),
                            (:UpperTriangular, 'U', 'N'),
                            (:UnitUpperTriangular, 'U', 'U'))
    @eval Base.LinAlg begin
        # Vector multiplication
        @deprecate A_mul_B!(A::$t{T,<:StridedMatrix}, b::StridedVector{T}) where {T<:BlasFloat}     mul!(A, b)
        @deprecate At_mul_B!(A::$t{T,<:StridedMatrix}, b::StridedVector{T}) where {T<:BlasFloat}    mul!(transpose(A), b)
        @deprecate Ac_mul_B!(A::$t{T,<:StridedMatrix}, b::StridedVector{T}) where {T<:BlasReal}     mul!(adjoint(A), b)
        @deprecate Ac_mul_B!(A::$t{T,<:StridedMatrix}, b::StridedVector{T}) where {T<:BlasComplex}  mul!(adjoint(A), b)

        # Matrix multiplication
        @deprecate A_mul_B!(A::$t{T,<:StridedMatrix}, B::StridedMatrix{T}) where {T<:BlasFloat}     mul!(A, B)
        @deprecate A_mul_B!(A::StridedMatrix{T}, B::$t{T,<:StridedMatrix}) where {T<:BlasFloat}     mul!(A, B)

        @deprecate At_mul_B!(A::$t{T,<:StridedMatrix}, B::StridedMatrix{T}) where {T<:BlasFloat}       mul!(transpose(A), B)
        @deprecate Ac_mul_B!(A::$t{T,<:StridedMatrix}, B::StridedMatrix{T}) where {T<:BlasComplex}     mul!(adjoint(A), B)
        @deprecate Ac_mul_B!(A::$t{T,<:StridedMatrix}, B::StridedMatrix{T}) where {T<:BlasReal}        mul!(adjoint(A), B)

        @deprecate A_mul_Bt!(A::StridedMatrix{T}, B::$t{T,<:StridedMatrix}) where {T<:BlasFloat}    mul!(A, transpose(B))
        @deprecate A_mul_Bc!(A::StridedMatrix{T}, B::$t{T,<:StridedMatrix}) where {T<:BlasComplex}  mul!(A, adjoint(B))
        @deprecate A_mul_Bc!(A::StridedMatrix{T}, B::$t{T,<:StridedMatrix}) where {T<:BlasReal}     mul!(A, adjoint(B))

        # Left division
        @deprecate A_ldiv_B!(A::$t{T,<:StridedMatrix}, B::StridedVecOrMat{T}) where {T<:BlasFloat}  ldiv!(A, B)
        @deprecate At_ldiv_B!(A::$t{T,<:StridedMatrix}, B::StridedVecOrMat{T}) where {T<:BlasFloat} ldiv!(transpose(A), B)
        @deprecate Ac_ldiv_B!(A::$t{T,<:StridedMatrix}, B::StridedVecOrMat{T}) where {T<:BlasReal}  ldiv!(adjoint(A), B)
        @deprecate Ac_ldiv_B!(A::$t{T,<:StridedMatrix}, B::StridedVecOrMat{T}) where {T<:BlasComplex}   ldiv!(adjoint(A), B)

        # Right division
        @deprecate A_rdiv_B!(A::StridedMatrix{T}, B::$t{T,<:StridedMatrix}) where {T<:BlasFloat}    rdiv!(A, B)
        @deprecate A_rdiv_Bt!(A::StridedMatrix{T}, B::$t{T,<:StridedMatrix}) where {T<:BlasFloat}   rdiv!(A, transpose(B))
        @deprecate A_rdiv_Bc!(A::StridedMatrix{T}, B::$t{T,<:StridedMatrix}) where {T<:BlasReal}    rdiv!(A, adjoint(B))
        @deprecate A_rdiv_Bc!(A::StridedMatrix{T}, B::$t{T,<:StridedMatrix}) where {T<:BlasComplex} rdiv!(A, adjoint(B))
    end
end

# A[ct]_(mul|ldiv|rdiv)_B[ct][!] methods from base/linalg/rowvector.jl, to deprecate
@eval Base.LinAlg begin
    @deprecate A_rdiv_Bt(rowvec::RowVector, mat::AbstractMatrix)    (/)(rowvec, transpose(mat))
    @deprecate A_rdiv_Bc(rowvec::RowVector, mat::AbstractMatrix)    (/)(rowvec, adjoint(mat))
    @deprecate At_ldiv_B(mat::AbstractMatrix, rowvec::RowVector)    (\)(transpose(mat), rowvec)
    @deprecate Ac_ldiv_B(mat::AbstractMatrix, rowvec::RowVector)    (\)(adjoint(mat), rowvec)
    @deprecate Ac_mul_B(u::RowVector, v::AbstractVector)        (*)(adjoint(u), v)
    @deprecate Ac_mul_B(vec::AbstractVector, mat::AbstractMatrix)   (*)(adjoint(vec), mat)
    @deprecate Ac_mul_B(rowvec1::RowVector, rowvec2::RowVector)     (*)(adjoint(rowvec1), rowvec2)
    @deprecate Ac_mul_B(vec::AbstractVector, rowvec::RowVector)     (*)(adjoint(vec), rowvec)
    @deprecate Ac_mul_B(vec1::AbstractVector, vec2::AbstractVector) (*)(adjoint(vec1), vec2)
    @deprecate Ac_mul_Bc(rowvec::RowVector, vec::AbstractVector)    (*)(adjoint(rowvec), adjoint(vec))
    @deprecate Ac_mul_Bc(vec::AbstractVector, mat::AbstractMatrix)  (*)(adjoint(vec), adjoint(mat))
    @deprecate Ac_mul_Bc(rowvec1::RowVector, rowvec2::RowVector)    (*)(adjoint(rowvec1), adjoint(rowvec2))
    @deprecate Ac_mul_Bc(vec::AbstractVector, rowvec::RowVector)    (*)(adjoint(vec), adjoint(rowvec))
    @deprecate Ac_mul_Bc(vec::AbstractVector, rowvec::AbstractVector)   (*)(adjoint(vec), adjoint(rowvec))
    @deprecate Ac_mul_Bc(mat::AbstractMatrix, rowvec::RowVector)        (*)(adjoint(mat), adjoint(rowvec))
    @deprecate A_mul_Bc(u::RowVector, v::AbstractVector)    (*)(u, adjoint(v))
    @deprecate A_mul_Bc(rowvec::RowVector, mat::AbstractMatrix) (*)(rowvec, adjoint(mat))
    @deprecate A_mul_Bc(rowvec1::RowVector, rowvec2::RowVector)     (*)(rowvec1, adjoint(rowvec2))
    @deprecate A_mul_Bc(vec::AbstractVector, rowvec::RowVector)     (*)(vec, adjoint(rowvec))
    @deprecate A_mul_Bc(vec1::AbstractVector, vec2::AbstractVector) (*)(vec1, adjoint(vec2))
    @deprecate A_mul_Bc(mat::AbstractMatrix, rowvec::RowVector)     (*)(mat, adjoint(rowvec))
    @deprecate At_mul_B(v::RowVector, u::AbstractVector)            (*)(transpose(v), u)
    @deprecate At_mul_B(vec::AbstractVector, mat::AbstractMatrix)   (*)(transpose(vec), mat)
    @deprecate At_mul_B(rowvec1::RowVector, rowvec2::RowVector)     (*)(transpose(rowvec1), rowvec2)
    @deprecate At_mul_B(vec::AbstractVector, rowvec::RowVector)     (*)(transpose(vec), rowvec)
    @deprecate At_mul_B(vec1::AbstractVector{T}, vec2::AbstractVector{T}) where {T<:Real}   (*)(transpose(vec1), vec2)
    @deprecate At_mul_B(vec1::AbstractVector, vec2::AbstractVector)     (*)(transpose(vec1), vec2)
    @deprecate At_mul_Bt(rowvec::RowVector, vec::AbstractVector)        (*)(transpose(rowvec), transpose(vec))
    @deprecate At_mul_Bt(vec::AbstractVector, mat::AbstractMatrix)      (*)(transpose(vec), transpose(mat))
    @deprecate At_mul_Bt(rowvec1::RowVector, rowvec2::RowVector)        (*)(transpose(rowvec1), transpose(rowvec2))
    @deprecate At_mul_Bt(vec::AbstractVector, rowvec::RowVector)        (*)(transpose(vec), transpose(rowvec))
    @deprecate At_mul_Bt(vec::AbstractVector, rowvec::AbstractVector)   (*)(transpose(vec), transpose(rowvec))
    @deprecate At_mul_Bt(mat::AbstractMatrix, rowvec::RowVector)    (*)(transpose(mat), transpose(rowvec))
    @deprecate A_mul_Bt(v::RowVector, A::AbstractVector)            (*)(v, transpose(A))
    @deprecate A_mul_Bt(rowvec::RowVector, mat::AbstractMatrix)     (*)(rowvec, transpose(mat))
    @deprecate A_mul_Bt(rowvec1::RowVector, rowvec2::RowVector)     (*)(rowvec1, transpose(rowvec2))
    @deprecate A_mul_Bt(vec::AbstractVector, rowvec::RowVector)     (*)(vec, transpose(rowvec))
    @deprecate A_mul_Bt(vec1::AbstractVector, vec2::AbstractVector) (*)(vec1, transpose(vec2))
    @deprecate A_mul_Bt(mat::AbstractMatrix, rowvec::RowVector)     (*)(mat, transpose(rowvec))
end

# A[ct]_(mul|ldiv|rdiv)_B[ct][!] methods from base/linalg/givens.jl, to deprecate
@eval Base.LinAlg begin
    @deprecate A_mul_Bc!(A::AbstractMatrix, R::Rotation)    mul!(A, adjoint(R))
    @deprecate A_mul_B!(R::Rotation, A::AbstractMatrix)     mul!(R, A)
    @deprecate A_mul_B!(G::Givens, R::Rotation)             mul!(G, R)
    @deprecate A_mul_Bc!(A::AbstractMatrix, G::Givens)      mul!(A, adjoint(G))
    @deprecate A_mul_B!(G::Givens, A::AbstractVecOrMat)     mul!(G, A)
    @deprecate A_mul_B!(G1::Givens, G2::Givens)             mul!(G1, G2)
    @deprecate A_mul_Bc(A::AbstractVecOrMat{T}, R::AbstractRotation{S}) where {T,S}     (*)(A, adjoint(R))
end


# methods involving RowVector from base/linalg/bidiag.jl, to deprecate
@eval Base.LinAlg begin
    \(::Diagonal, ::RowVector) = _mat_ldiv_rowvec_error()
    \(::Bidiagonal, ::RowVector) = _mat_ldiv_rowvec_error()
    \(::Bidiagonal{<:Number}, ::RowVector{<:Number}) = _mat_ldiv_rowvec_error()
    \(::Adjoint{<:Any,<:Bidiagonal}, ::RowVector) = _mat_ldiv_rowvec_error()
    \(::Transpose{<:Any,<:Bidiagonal}, ::RowVector) = _mat_ldiv_rowvec_error()
    \(::Adjoint{<:Number,<:Bidiagonal{<:Number}}, ::RowVector{<:Number}) = _mat_ldiv_rowvec_error()
    \(::Transpose{<:Number,<:Bidiagonal{<:Number}}, ::RowVector{<:Number}) = _mat_ldiv_rowvec_error()
    _mat_ldiv_rowvec_error() = throw(DimensionMismatch("Cannot left-divide matrix by transposed vector"))
end

# methods involving RowVector from base/linalg/diagonal.jl, to deprecate
@eval Base.LinAlg begin
    *(rowvec::RowVector, D::Diagonal) = rvtranspose(D * rvtranspose(rowvec)) # seems potentially incorrect without also transposing D?
    *(D::Diagonal, transrowvec::Transpose{<:Any,<:RowVector}) = (rowvec = transrowvec.parent; D*rvtranspose(rowvec))
    *(D::Diagonal, adjrowvec::Adjoint{<:Any,<:RowVector}) = (rowvec = adjrowvec.parent; D*rvadjoint(rowvec))
end

# methods involving RowVector from base/linalg/qr.jl, to deprecate
@eval Base.LinAlg begin
    *(rowvec::RowVector, adjB::Adjoint{<:Any,<:AbstractQ}) = (B = adjB.parent; rvadjoint(B*rvadjoint(rowvec)))
end

# methods involving RowVector from base/linalg/qr.jl, to deprecate
@eval Base.LinAlg begin
    *(A::RowVector, B::Adjoint{<:Any,<:AbstractRotation}) = A * adjoint(B.parent)
end

# methods involving RowVector from base/linalg/generic.jl, to deprecate
@eval Base.LinAlg begin
    """
        norm(A::RowVector, q::Real=2)

    For row vectors, return the ``q``-norm of `A`, which is equivalent to the p-norm with
    value `p = q/(q-1)`. They coincide at `p = q = 2`.

    The difference in norm between a vector space and its dual arises to preserve
    the relationship between duality and the inner product, and the result is
    consistent with the p-norm of `1 × n` matrix.

    # Examples
    ```jldoctest
    julia> v = [1; im];

    julia> vc = RowVector(v);

    julia> norm(vc, 1)
    1.0

    julia> norm(v, 1)
    2.0

    julia> norm(vc, 2)
    1.4142135623730951

    julia> norm(v, 2)
    1.4142135623730951

    julia> norm(vc, Inf)
    2.0

    julia> norm(v, Inf)
    1.0
    ```
    """
    norm(tv::RowVector, q::Real) = q == Inf ? norm(rvtranspose(tv), 1) : norm(rvtranspose(tv), q/(q-1))
    norm(tv::RowVector) = norm(rvtranspose(tv))
end

# methods involving RowVector from base/linalg/factorization.jl, to deprecate
@eval Base.LinAlg begin
    \(A::Adjoint{<:Any,<:Factorization}, B::RowVector) = adjoint(A.parent) \ B
    \(A::Transpose{<:Any,<:Factorization}, B::RowVector) = transpose(A.parent) \ B
    \(A::Transpose{<:Any,<:Factorization{<:Real}}, B::RowVector) = transpose(A.parent) \ B
end

# methods involving RowVector from base/linalg/symmetric.jl, to deprecate
@eval Base.LinAlg begin
    *(A::RowVector, transB::Transpose{<:Any,<:RealHermSymComplexSym}) = A * transB.parent
    *(A::RowVector, adjB::Adjoint{<:Any,<:RealHermSymComplexHerm}) = A * adjB.parent
    \(A::HermOrSym{<:Any,<:StridedMatrix}, B::RowVector) = invoke(\, Tuple{AbstractMatrix, RowVector}, A, B)
    *(A::Adjoint{<:Any,<:RealHermSymComplexHerm}, B::Adjoint{<:Any,<:RowVector}) = A.parent * B
    *(A::Adjoint{<:Any,<:RealHermSymComplexHerm}, B::Transpose{<:Any,<:RowVector}) = A.parent * B
    *(A::Transpose{<:Any,<:RealHermSymComplexSym}, B::Adjoint{<:Any,<:RowVector}) = A.parent * B
    *(A::Transpose{<:Any,<:RealHermSymComplexSym}, B::Transpose{<:Any,<:RowVector}) = A.parent * B
end

# methods involving RowVector from base/linalg/triangular.jl, to deprecate
@eval Base.LinAlg begin
    *(rowvec::RowVector, A::AbstractTriangular) = rvtranspose(transpose(A) * rvtranspose(rowvec))
    *(rowvec::RowVector, transA::Transpose{<:Any,<:AbstractTriangular}) = rvtranspose(transA.parent * rvtranspose(rowvec))
    *(A::AbstractTriangular, transrowvec::Transpose{<:Any,<:RowVector}) = A * rvtranspose(transrowvec.parent)
    *(transA::Transpose{<:Any,<:AbstractTriangular}, transrowvec::Transpose{<:Any,<:RowVector}) = transA * rvtranspose(transrowvec.parent)
    *(rowvec::RowVector, adjA::Adjoint{<:Any,<:AbstractTriangular}) = rvadjoint(adjA.parent * rvadjoint(rowvec))
    *(A::AbstractTriangular, adjrowvec::Adjoint{<:Any,<:RowVector}) = A * rvadjoint(adjrowvec.parent)
    *(adjA::Adjoint{<:Any,<:AbstractTriangular}, adjrowvec::Adjoint{<:Any,<:RowVector}) = adjA * rvadjoint(adjrowvec.parent)
    \(::Union{UpperTriangular,LowerTriangular}, ::RowVector) = throw(DimensionMismatch("Cannot left-divide matrix by transposed vector"))
    \(::Union{UnitUpperTriangular,UnitLowerTriangular}, ::RowVector) = throw(DimensionMismatch("Cannot left-divide matrix by transposed vector"))
    \(::Adjoint{<:Any,<:Union{UpperTriangular,LowerTriangular}}, ::RowVector) = throw(DimensionMismatch("Cannot left-divide matrix by transposed vector"))
    \(::Adjoint{<:Any,<:Union{UnitUpperTriangular,UnitLowerTriangular}}, ::RowVector) = throw(DimensionMismatch("Cannot left-divide matrix by transposed vector"))
    \(::Transpose{<:Any,<:Union{UpperTriangular,LowerTriangular}}, ::RowVector) = throw(DimensionMismatch("Cannot left-divide matrix by transposed vector"))
    \(::Transpose{<:Any,<:Union{UnitUpperTriangular,UnitLowerTriangular}}, ::RowVector) = throw(DimensionMismatch("Cannot left-divide matrix by transposed vector"))
    /(rowvec::RowVector, A::Union{UpperTriangular,LowerTriangular}) = rvtranspose(transpose(A) \ rvtranspose(rowvec))
    /(rowvec::RowVector, A::Union{UnitUpperTriangular,UnitLowerTriangular}) = rvtranspose(transpose(A) \ rvtranspose(rowvec))
    /(rowvec::RowVector, transA::Transpose{<:Any,<:Union{UpperTriangular,LowerTriangular}}) = rvtranspose(transA.parent \ rvtranspose(rowvec))
    /(rowvec::RowVector, transA::Transpose{<:Any,<:Union{UnitUpperTriangular,UnitLowerTriangular}}) = rvtranspose(transA.parent \ rvtranspose(rowvec))
    /(rowvec::RowVector, adjA::Adjoint{<:Any,<:Union{UpperTriangular,LowerTriangular}}) = /(rowvec, adjoint(adjA.parent))
    /(rowvec::RowVector, adjA::Adjoint{<:Any,<:Union{UnitUpperTriangular,UnitLowerTriangular}}) = /(rowvec, adjoint(adjA.parent))
    *(A::Adjoint{<:Any,<:AbstractTriangular}, B::Transpose{<:Any,<:RowVector}) = A * rvtranspose(B.parent)
    *(A::Transpose{<:Any,<:AbstractTriangular}, B::Adjoint{<:Any,<:RowVector}) = A * rvadjoint(B.parent)
end

# issue #24822
@deprecate_binding Display AbstractDisplay

# 24595
@deprecate falses(A::AbstractArray) falses(size(A))
@deprecate trues(A::AbstractArray) trues(size(A))

# issue #24794
@deprecate linspace(start, stop)     linspace(start, stop, 50)
@deprecate logspace(start, stop)     logspace(start, stop, 50)

@deprecate merge!(repo::LibGit2.GitRepo, args...; kwargs...) LibGit2.merge!(repo, args...; kwargs...)

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
    print_with_color(info_color(), iob, prefix; bold = true)
    println_with_color(info_color(), iob, chomp(string(msg...)))
    print(io, String(take!(buf)))
    return
end
info(msg...; prefix="INFO: ") = info(STDERR, msg..., prefix=prefix)

# print a warning only once

const have_warned = Set()

warn_once(io::IO, msg...) = warn(io, msg..., once=true)
warn_once(msg...) = warn(STDERR, msg..., once=true)

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
    print_with_color(warn_color(), iob, prefix; bold = true)
    print_with_color(warn_color(), iob, str)
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
warn(msg...; kw...) = warn(STDERR, msg...; kw...)

warn(io::IO, err::Exception; prefix="ERROR: ", kw...) =
    warn(io, sprint(showerror, err), prefix=prefix; kw...)

warn(err::Exception; prefix="ERROR: ", kw...) =
    warn(STDERR, err, prefix=prefix; kw...)

info(io::IO, err::Exception; prefix="ERROR: ", kw...) =
    info(io, sprint(showerror, err), prefix=prefix; kw...)

info(err::Exception; prefix="ERROR: ", kw...) =
    info(STDERR, err, prefix=prefix; kw...)

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

# PR #24594
@eval LibGit2 begin
    @deprecate AbstractCredentials AbstractCredential false
    @deprecate UserPasswordCredentials UserPasswordCredential false
    @deprecate SSHCredentials SSHCredential false
end

# issue #24804
@deprecate_moved sum_kbn "KahanSummation"
@deprecate_moved cumsum_kbn "KahanSummation"

# PR #25249: SparseArrays to stdlib
## the Base.SparseArrays module itself and exported types are deprecated in base/sysimg.jl
## functions that were re-exported from Base
@deprecate_moved nonzeros   "SparseArrays" true true
@deprecate_moved permute    "SparseArrays" true true
@deprecate_moved blkdiag    "SparseArrays" true true
@deprecate_moved dropzeros  "SparseArrays" true true
@deprecate_moved dropzeros! "SparseArrays" true true
@deprecate_moved issparse   "SparseArrays" true true
@deprecate_moved sparse     "SparseArrays" true true
@deprecate_moved sparsevec  "SparseArrays" true true
@deprecate_moved spdiagm    "SparseArrays" true true
@deprecate_moved sprand     "SparseArrays" true true
@deprecate_moved sprandn    "SparseArrays" true true
@deprecate_moved spzeros    "SparseArrays" true true
@deprecate_moved rowvals    "SparseArrays" true true
@deprecate_moved nzrange    "SparseArrays" true true
@deprecate_moved nnz        "SparseArrays" true true
## functions that were exported from Base.SparseArrays but not from Base
@deprecate_moved droptol!   "SparseArrays" false true
## deprecated functions that are moved to stdlib/SparseArrays/src/deprecated.jl
@deprecate_moved spones     "SparseArrays" true true
@deprecate_moved speye      "SparseArrays" true true


# PR #25021
@deprecate_moved normalize_string "Unicode" true true
@deprecate_moved graphemes "Unicode" true true
@deprecate_moved is_assigned_char "Unicode" true true

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

# issue #25016
@deprecate lpad(s, n::Integer, p) lpad(string(s), n, string(p))
@deprecate rpad(s, n::Integer, p) rpad(string(s), n, string(p))

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

# PR #25184. Use getproperty instead of getindex for Factorizations
function getindex(F::Factorization, s::Symbol)
    depwarn("`F[:$s]` is deprecated, use `F.$s` instead.", :getindex)
    return getproperty(F, s)
end
@eval Base.LinAlg begin
    @deprecate getq(F::Factorization) F.Q
end

# Issues #17812 Remove default stride implementation
function strides(a::AbstractArray)
    depwarn("""
    `strides(a::AbstractArray)` is deprecated for general arrays.
    Specialize `strides` for custom array types that have the appropriate representation in memory.
    Warning: inappropriately implementing this method for an array type that does not use strided
    storage may lead to incorrect results or segfaults.
    """, :strides)
    size_to_strides(1, size(a)...)
end

@deprecate substrides(s, parent, dim, I::Tuple) substrides(parent, strides(parent), I)

@deprecate lexcmp(x::AbstractArray, y::AbstractArray) cmp(x, y)
@deprecate lexcmp(x::Real, y::Real)                   cmp(isless, x, y)
@deprecate lexcmp(x::Complex, y::Complex)             cmp((real(x),imag(x)), (real(y),imag(y)))
@deprecate lexcmp(x, y)                               cmp(x, y)

@deprecate lexless isless

@deprecate_binding iteratorsize IteratorSize
@deprecate_binding iteratoreltype IteratorEltype

@deprecate search(str::Union{String,SubString}, re::Regex, idx::Integer) findnext(re, str, idx)
@deprecate search(s::AbstractString, r::Regex, idx::Integer) findnext(r, s, idx)
@deprecate search(s::AbstractString, r::Regex) findfirst(r, s)
@deprecate search(s::AbstractString, c::Char, i::Integer) findnext(equalto(c), s, i)
@deprecate search(s::AbstractString, c::Char) findfirst(equalto(c), s)
@deprecate search(a::ByteArray, b::Union{Int8,UInt8}, i::Integer) findnext(equalto(b), a, i)
@deprecate search(a::ByteArray, b::Union{Int8,UInt8}) findfirst(equalto(b), a)
@deprecate search(a::String, b::Union{Int8,UInt8}, i::Integer) findnext(equalto(b), unsafe_wrap(Vector{UInt8}, a), i)
@deprecate search(a::String, b::Union{Int8,UInt8}) findfirst(equalto(b), unsafe_wrap(Vector{UInt8}, a))
@deprecate search(a::ByteArray, b::Char, i::Integer) findnext(equalto(UInt8(b)), a, i)
@deprecate search(a::ByteArray, b::Char) findfirst(equalto(UInt8(b)), a)

@deprecate search(s::AbstractString, c::Union{Tuple{Vararg{Char}},AbstractVector{Char},Set{Char}}, i::Integer) findnext(occursin(c), s, i)
@deprecate search(s::AbstractString, c::Union{Tuple{Vararg{Char}},AbstractVector{Char},Set{Char}}) findfirst(occursin(c), s)
@deprecate search(s::AbstractString, t::AbstractString, i::Integer) findnext(t, s, i)
@deprecate search(s::AbstractString, t::AbstractString) findfirst(t, s)

@deprecate search(buf::IOBuffer, delim::UInt8) findfirst(equalto(delim), buf)
@deprecate search(buf::Base.GenericIOBuffer, delim::UInt8) findfirst(equalto(delim), buf)

@deprecate rsearch(s::AbstractString, c::Union{Tuple{Vararg{Char}},AbstractVector{Char},Set{Char}}, i::Integer) findprev(occursin(c), s, i)
@deprecate rsearch(s::AbstractString, c::Union{Tuple{Vararg{Char}},AbstractVector{Char},Set{Char}}) findlast(occursin(c), s)
@deprecate rsearch(s::AbstractString, t::AbstractString, i::Integer) findprev(t, s, i)
@deprecate rsearch(s::AbstractString, t::AbstractString) findlast(t, s)
@deprecate rsearch(s::ByteArray, t::ByteArray, i::Integer) findprev(t, s, i)
@deprecate rsearch(s::ByteArray, t::ByteArray) findlast(t, s)

@deprecate rsearch(str::Union{String,SubString}, re::Regex, idx::Integer) findprev(re, str, idx)
@deprecate rsearch(str::Union{String,SubString}, re::Regex) findlast(re, str)
@deprecate rsearch(s::AbstractString, r::Regex, idx::Integer) findprev(r, s, idx)
@deprecate rsearch(s::AbstractString, r::Regex) findlast(r, s)
@deprecate rsearch(s::AbstractString, c::Char, i::Integer) findprev(equalto(c), s, i)
@deprecate rsearch(s::AbstractString, c::Char) findlast(equalto(c), s)
@deprecate rsearch(a::Union{String,ByteArray}, b::Union{Int8,UInt8}, i::Integer = endof(a)) findprev(equalto(b), a, i)
@deprecate rsearch(a::String, b::Union{Int8,UInt8}, i::Integer = endof(a)) findprev(equalto(Char(b)), a, i)
@deprecate rsearch(a::ByteArray, b::Char, i::Integer = endof(a)) findprev(equalto(UInt8(b)), a, i)

@deprecate searchindex(s::AbstractString, t::AbstractString) first(findfirst(t, s))
@deprecate searchindex(s::AbstractString, t::AbstractString, i::Integer) first(findnext(t, s, i))
@deprecate rsearchindex(s::AbstractString, t::AbstractString) first(findlast(t, s))
@deprecate rsearchindex(s::AbstractString, t::AbstractString, i::Integer) first(findprev(t, s, i))

@deprecate searchindex(s::AbstractString, c::Char) findfirst(equalto(c), s)
@deprecate searchindex(s::AbstractString, c::Char, i::Integer) findnext(equalto(c), s, i)
@deprecate rsearchindex(s::AbstractString, c::Char) findlast(equalto(c), s)
@deprecate rsearchindex(s::AbstractString, c::Char, i::Integer) findprev(equalto(c), s, i)

@deprecate ismatch(r::Regex, s::AbstractString) contains(s, r)

@deprecate findin(a, b) find(occursin(b), a)


# END 0.7 deprecations

# BEGIN 1.0 deprecations

# END 1.0 deprecations
