__precompile__()

module Compat

using Base.Meta

# for compatibility with legacy code that uses Compat.<symbol> to remain
# compatible with v0.3; these symbols are now identical to those in Base.
import Base: unsafe_convert, @irrational

macro Dict(pairs...)
    esc(Expr(:call, :Dict, pairs...))
end
macro AnyDict(pairs...)
    esc(Expr(:call, :(Base.AnyDict), pairs...))
end

"""Get just the function part of a function declaration."""
withincurly(ex) = isexpr(ex, :curly) ? ex.args[1] : ex

include("ngenerate.jl")
using .CompatCartesian
export @ngenerate, @nsplat

if VERSION < v"0.5.0-dev+961"
    export walkdir

    function walkdir(root; topdown=true, follow_symlinks=false, onerror=throw)
        content = nothing
        try
            content = readdir(root)
        catch err
            isa(err, SystemError) || throw(err)
            onerror(err)
            #Need to return an empty task to skip the current root folder
            return Task(()->())
        end
        dirs = Array(eltype(content), 0)
        files = Array(eltype(content), 0)
        for name in content
            if isdir(joinpath(root, name))
                push!(dirs, name)
            else
                push!(files, name)
            end
        end

        function _it()
            if topdown
                produce(root, dirs, files)
            end
            for dir in dirs
                path = joinpath(root,dir)
                if follow_symlinks || !islink(path)
                    for (root_l, dirs_l, files_l) in walkdir(path, topdown=topdown, follow_symlinks=follow_symlinks, onerror=onerror)
                        produce(root_l, dirs_l, files_l)
                    end
                end
            end
            if !topdown
                produce(root, dirs, files)
            end
        end
        Task(_it)
    end
end

function rewrite_show(ex)
    if isexpr(ex, :call)
        Expr(:call, rewrite_show(ex.args[1]), ex.args[2:end]...)
    elseif isexpr(ex, :curly)
        Expr(:curly, rewrite_show(ex.args[1]), ex.args[2:end]...)
    else
        :(Base.writemime)
    end
end

function is_quote_symbol(ex::ANY, val::Symbol)
    if isa(ex, QuoteNode)
        return (ex::QuoteNode).value === val
    elseif isa(ex, Expr)
        ex = ex::Expr
        return ex.head === :quote && length(ex.args) == 1 && ex.args[1] === val
    end
    return false
end

# rewrites accesses to IOContext dicts
function rewrite_iocontext!(expr::Expr)
    args = expr.args
    nargs = length(args)
    if nargs == 4 && expr.head === :call && args[1] === :get && args[4] === false
        key = args[3]
        if is_quote_symbol(key, :limit) || is_quote_symbol(key, :compact)
            if VERSION >= v"0.5.0-dev+1936" && VERSION < v"0.5.0-dev+4305"
                args[1] = :(Base.limit_output)
                deleteat!(args, 3:4)
            elseif VERSION < v"0.5.0-dev+1936"
                expr.head = :quote
                args[1] = false
                deleteat!(args, 3:4)
            end
        elseif is_quote_symbol(key, :multiline)
            if VERSION < v"0.5.0-dev+4305"
                expr.head = :quote
                args[1] = false
                deleteat!(args, 3:4)
            end
        end
    end
end

if VERSION < v"0.5.0-dev+3831"
    Base.Symbol(args...) = symbol(args...)::Symbol
end

if VERSION < v"0.5.0-dev+2396"
    function new_style_call_overload(ex::Expr)
        # Not a function call
        ((ex.head === :(=) || ex.head === :function) &&
         length(ex.args) == 2 && isexpr(ex.args[1], :call)) || return false
        callee = (ex.args[1]::Expr).args[1]
        # Only Expr function name can be call overload
        isa(callee, Expr) || return false
        callee = callee::Expr
        # (a::A)() = ...
        callee.head === :(::) && return true
        # The other case is with type parameter.
        # Filter out everything without one.
        (callee.head === :curly && length(callee.args) >= 1) || return false
        # Check what the type parameter applies to is a Expr(:(::))
        return isexpr(callee.args[1], :(::))
    end
else
    new_style_call_overload(ex::Expr) = false
end

istopsymbol(ex, mod, sym) = ex in (sym, Expr(:(.), mod, Expr(:quote, sym)))

function _compat(ex::Expr)
    if ex.head === :call
        if VERSION < v"0.5.0-dev+4340" && length(ex.args) > 3 &&
            istopsymbol(withincurly(ex.args[1]), :Base, :show)
            ex = rewrite_show(ex)
        end
        if VERSION < v"0.5.0-dev+4305"
            rewrite_iocontext!(ex)
        end
    elseif ex.head === :curly
        f = ex.args[1]
    elseif ex.head === :macrocall
        f = ex.args[1]
        if VERSION < v"0.5.0-dev+2129" && f === symbol("@boundscheck") && length(ex.args) == 2
            # Handle 0.5 single argument @boundscheck syntax. (0.4 has a two
            # arguments and a very diffferent meaning).  `nothing` is included
            # so we have a consistent return type.
            ex = :($(ex.args[2]); nothing)
        end
    elseif ex.head === :quote && isa(ex.args[1], Symbol)
        # Passthrough
        return ex
    elseif new_style_call_overload(ex)
        callexpr = ex.args[1]::Expr
        callee = callexpr.args[1]::Expr
        is_kw = (length(callexpr.args) >= 2 &&
                 isexpr(callexpr.args[2], :parameters))
        if callee.head === :(::)
            # (:function, (:call, :(:(::), <1>), <2>), <body>) ->
            # (:function, (:call, :(Base.call), :(:(::), <1>), <2>), <body>)
            unshift!(callexpr.args, :(Base.call))
        else
            # (:function, (:call, :(curly, :(:(::), <1>), <3>), <2>), <body>) ->
            # (:function, (:call, :(curly, :(Base.call), <3>), :(:(::), <1>), <2>), <body>)
            obj = callee.args[1]::Expr
            callee.args[1] = :(Base.call)
            insert!(callexpr.args, 2, obj)
        end
        if is_kw
            # Expr(:parameters) is moved to the 3rd argument
            params = callexpr.args[3]
            @assert isexpr(params, :parameters)
            obj = callexpr.args[2]
            callexpr.args[2] = params
            callexpr.args[3] = obj
        end
    elseif VERSION < v"0.5.0-dev+4002" && ex.head == :. && length(ex.args) == 2 # 15032
        if isexpr(ex.args[2], :quote) && isa(ex.args[2].args[1], QuoteNode)
            # foo.:bar -> foo.(:bar) in older Julia
            return Expr(ex.head, _compat(ex.args[1]), ex.args[2].args[1])
        elseif isexpr(ex.args[2], :quote) && isexpr(ex.args[2].args[1], :quote) &&
               isa(ex.args[2].args[1].args[1], Symbol)
            # foo.:bar -> foo.(:bar) in older Julia
            return Expr(ex.head, _compat(ex.args[1]), QuoteNode(ex.args[2].args[1].args[1]))
        elseif isexpr(ex.args[2], :tuple)
            # f.(arg1, arg2...) -> broadcast(f, arg1, arg2...)
            return Expr(:call, :broadcast, _compat(ex.args[1]), map(_compat, ex.args[2].args)...)
        elseif !isa(ex.args[2], QuoteNode) &&
               !(isexpr(ex.args[2], :quote) && isa(ex.args[2].args[1], Symbol))
            # f.(arg) -> broadcast(f, arg)
            return Expr(:call, :broadcast, _compat(ex.args[1]), _compat(ex.args[2]))
        end
    elseif ex.head === :import
        if VERSION < v"0.5.0-dev+4340" && length(ex.args) == 2 && ex.args[1] === :Base && ex.args[2] === :show
            return quote
                import Base.show
                import Base.writemime
            end
        end
    end
    return Expr(ex.head, map(_compat, ex.args)...)
end
function _compat(ex::Symbol)
    if VERSION < v"0.5.0-dev+1343" && (ex == :Future || ex == :RemoteChannel)
        return :RemoteRef
    end
    return ex
end
_compat(ex) = ex

macro compat(ex)
    esc(_compat(ex))
end

export @compat

import Base: remotecall, remotecall_fetch, remotecall_wait, remote_do
if VERSION < v"0.5.0-dev+431"
    for f in (:remotecall, :remotecall_fetch, :remotecall_wait, :remote_do)
        @eval begin
            ($f)(f::Function, w::Base.LocalProcess, args...)   = ($f)(w, f, args...)
            ($f)(f::Function, w::Base.Worker, args...)         = ($f)(w, f, args...)
            ($f)(f::Function, id::Integer, args...)            = ($f)(id, f, args...)
        end
    end
end

if VERSION < v"0.5.0-dev+763"
    export SparseArrays
    const SparseArrays = Base.SparseMatrix
end

if VERSION < v"0.5.0-dev+1229"
    const Filesystem = Base.FS
else
    import Base.Filesystem
end

if VERSION < v"0.5.0-dev+1946"
    const supertype = super
    export supertype
end

if VERSION < v"0.5.0-dev+679"
    import Base: cov, cor

    cov(x::AbstractVector, corrected::Bool) = cov(x, corrected=corrected)
    cov(X::AbstractMatrix, vardim::Integer) = cov(X, vardim=vardim)
    cov(X::AbstractMatrix, vardim::Integer, corrected::Bool) = cov(X, vardim=vardim, corrected=corrected)
    cov(x::AbstractVector, y::AbstractVector, corrected::Bool) = cov(x, y, corrected=corrected)
    cov(X::AbstractMatrix, Y::AbstractMatrix, vardim::Integer) = cov(X, Y, vardim=vardim)
    cov(X::AbstractMatrix, Y::AbstractMatrix, vardim::Integer, corrected::Bool) = cov(X, Y, vardim=vardim, corrected=corrected)

    cor(X::AbstractMatrix, vardim::Integer) = cor(X, vardim=vardim)
    cor(X::AbstractMatrix, Y::AbstractMatrix, vardim::Integer) = cor(X, Y, vardim=vardim)
end

if VERSION < v"0.5.0-dev+2228"
    const readstring = readall
    export readstring

    Base.read(s::IO) = readbytes(s)
    Base.read(s::IO, nb) = readbytes(s, nb)

    Base.write(filename::AbstractString, args...) = open(io->write(io, args...), filename, "w")

    Base.read(filename::AbstractString, args...) = open(io->read(io, args...), filename)
    Base.read!(filename::AbstractString, a) = open(io->read!(io, a), filename)
    Base.readuntil(filename::AbstractString, args...) = open(io->readuntil(io, args...), filename)
    Base.readline(filename::AbstractString) = open(readline, filename)
    Base.readlines(filename::AbstractString) = open(readlines, filename)
    Base.readavailable(s::IOStream) = read!(s, @compat Vector{UInt8}(nb_available(s)))
    Base.readavailable(s::IOBuffer) = read(s)

    function Base.write(to::IO, from::IO)
        while !eof(from)
            write(to, readavailable(from))
        end
    end

    function Base.eachline(filename::AbstractString)
        s = open(filename)
        EachLine(s, ()->close(s))
    end
end

if VERSION < v"0.5.0-dev+2023"
    const displaysize = Base.tty_size
    export displaysize
end


# Pull Request https://github.com/JuliaLang/julia/pull/13232
# Rounding and precision functions:

if VERSION >= v"0.5.0-dev+1182"
    import Base:
        setprecision, setrounding, rounding

else  # if VERSION < v"0.5.0-dev+1182"

    export setprecision
    export setrounding
    export rounding

    setprecision(f, ::Type{BigFloat}, prec) = with_bigfloat_precision(f, prec)
    setprecision(::Type{BigFloat}, prec) = set_bigfloat_precision(prec)

    # assume BigFloat if type not explicit:
    setprecision(prec) = setprecision(BigFloat, prec)
    setprecision(f, prec) = setprecision(f, BigFloat, prec)

    Base.precision(::Type{BigFloat}) = get_bigfloat_precision()

    setrounding(f, T, rounding_mode) =
        with_rounding(f, T, rounding_mode)

    setrounding(T, rounding_mode) = set_rounding(T, rounding_mode)

    rounding(T) = get_rounding(T)

end

module LinAlg
    if VERSION < v"0.5.0-dev+2022"
        const checksquare = Base.LinAlg.chksquare
    else
        import Base.LinAlg.checksquare
    end
end

if VERSION < v"0.5.0-dev+2915"
    const issymmetric = issym
    export issymmetric
end

if VERSION < v"0.5.0-dev+977"
    export foreach

    foreach(f) = (f(); nothing)
    foreach(f, itr) = (for x in itr; f(x); end; nothing)
    foreach(f, itrs...) = (for z in zip(itrs...); f(z...); end; nothing)
end

if !isdefined(Base, :istextmime)
    export istextmime
    istextmime(m::@compat(Union{MIME,AbstractString})) = istext(m)
end

export @functorize
macro functorize(f)
    if VERSION >= v"0.5.0-dev+3701"
        f === :scalarmax       ? :(Base.scalarmax) :
        f === :scalarmin       ? :(Base.scalarmin) :
        f === :centralizedabs2fun ? :(typeof(Base.centralizedabs2fun(0)).name.primary) :
        f
    else
        f = f === :identity        ? :(Base.IdFun()) :
            f === :abs             ? :(Base.AbsFun()) :
            f === :abs2            ? :(Base.Abs2Fun()) :
            f === :exp             ? :(Base.ExpFun()) :
            f === :log             ? :(Base.LogFun()) :
            f === :&               ? :(Base.AndFun()) :
            f === :|               ? :(Base.OrFun()) :
            f === :+               ? :(Base.AddFun()) :
            f === :*               ? :(Base.MulFun()) :
            f === :scalarmax       ? :(Base.MaxFun()) :
            f === :scalarmin       ? :(Base.MinFun()) :
            f === :centralizedabs2fun ? :(Base.CentralizedAbs2Fun) :
            f === :<               ? :(Base.LessFun()) :
            f === :>               ? :(Base.MoreFun()) :
            f === :conj            ? :(Base.ConjFun()) :
            f === :-               ? :(Base.SubFun()) :
            f === :^               ? :(Base.PowFun()) :
            f === :/               ? :(Base.RDivFun()) :
            f === :\               ? :(Base.LDivFun()) :
            f === :div             ? :(Base.IDivFun()) :
            f === :$               ? :(Base.XorFun()) :
            f === :.+              ? :(Base.DotAddFun()) :
            f === :.-              ? :(Base.DotSubFun()) :
            f === :.*              ? :(Base.DotMulFun()) :
            f === :mod             ? :(Base.ModFun()) :
            f === :rem             ? :(Base.RemFun()) :
            # DotRemFun is defined, but ::call(::DotRemFun, ...) is not until later
            #f === :.%              ? :(Base.DotRemFun()) :
            f === :.<<             ? :(Base.DotLSFun()) :
            f === :.>>             ? :(Base.DotRSFun()) :
            f === :./              ? :(Base.DotRDivFun()) :
            f === :max             ? :(Base.ElementwiseMaxFun()) :
            f === :min             ? :(Base.ElementwiseMinFun()) :
            f
        if VERSION >= v"0.5.0-dev+741"
            f = f === :complex     ? :(Base.SparseArrays.ComplexFun()) :
                f === :dot         ? :(Base.SparseArrays.DotFun()) :
                f
        end
        if VERSION >= v"0.5.0-dev+1472"
            f = f === Symbol(".÷") ? :(Base.DotIDivFun()) :
                f === :.%          ? :(Base.DotRemFun()) :
                f
        end
        f
    end
end

if !isdefined(Base, :Threads)
    @eval module Threads
        macro threads(expr)
            return esc(expr)
        end
        threadid() = 1
        nthreads() = 1
        export @threads, threadid, nthreads
    end
    export Threads
end

if !isdefined(Base, :normalize)
    function normalize!(v::AbstractVector, p::Real=2)
        nrm = norm(v, p)
        __normalize!(v, nrm)
    end

    @inline function __normalize!(v::AbstractVector, nrm::AbstractFloat)
        #The largest positive floating point number whose inverse is less than
        #infinity
        δ = inv(prevfloat(typemax(nrm)))
        if nrm ≥ δ #Safe to multiply with inverse
            invnrm = inv(nrm)
            scale!(v, invnrm)
        else # scale elements to avoid overflow
            εδ = eps(one(nrm))/δ
            scale!(v, εδ)
            scale!(v, inv(nrm*εδ))
        end
        v
    end

    copy_oftype{T,N}(A::AbstractArray{T,N}, ::Type{T}) = copy(A)
    copy_oftype{T,N,S}(A::AbstractArray{T,N}, ::Type{S}) = convert(AbstractArray{S,N}, A)

    function normalize(v::AbstractVector, p::Real = 2)
        nrm = norm(v, p)
        if !isempty(v)
            vv = copy_oftype(v, typeof(v[1]/nrm))
            return __normalize!(vv, nrm)
        else
            T = typeof(zero(eltype(v))/nrm)
            return T[]
        end
    end

    export normalize, normalize!
end

if !isdefined(Base, :AsyncCondition)
    type AsyncCondition
        cond::Condition
        handle::Ptr{Void}

        function AsyncCondition(func=nothing)
            this = new(Condition())
            # the callback is supposed to be called with this AsyncCondition
            # as the argument, so we need to create a wrapper callback
            if func == nothing
                function wrapfunc(data)
                    notify(this.cond)

                    nothing
                end
            else
                function wrapfunc(data)
                    notify(this.cond)
                    func(this)

                    nothing
                end
            end
            work = Base.SingleAsyncWork(wrapfunc)
            this.handle = work.handle

            this
        end
    end

    Base.wait(c::AsyncCondition) = wait(c.cond)
else
    import Base.AsyncCondition
end

# 0.5.0-dev+2301, JuliaLang/julia#14766
if !isdefined(Base, :unsafe_write)
    const unsafe_write = write
    export unsafe_write
end

# JuliaLang/julia#16219
if !isdefined(Base, @compat Symbol("@static"))
     macro static(ex)
        if isa(ex, Expr)
            if ex.head === :if
                cond = eval(current_module(), ex.args[1])
                if cond
                    return esc(ex.args[2])
                elseif length(ex.args) == 3
                    return esc(ex.args[3])
                else
                    return nothing
                end
            end
        end
        throw(ArgumentError("invalid @static macro"))
    end
    export @static
end

# JuliaLang/julia#14082
if VERSION < v"0.5.0-dev+4295"
    function repeat(A::AbstractArray;
                    inner=ntuple(x->1, ndims(A)),
                    outer=ntuple(x->1, ndims(A)))
        ndims_in = ndims(A)
        length_inner = length(inner)
        length_outer = length(outer)

        length_inner >= ndims_in || throw(ArgumentError("number of inner repetitions ($(length(inner))) cannot be less than number of dimensions of input ($(ndims(A)))"))
        length_outer >= ndims_in || throw(ArgumentError("number of outer repetitions ($(length(outer))) cannot be less than number of dimensions of input ($(ndims(A)))"))

        ndims_out = max(ndims_in, length_inner, length_outer)

        inner = vcat(collect(inner), ones(Int,ndims_out-length_inner))
        outer = vcat(collect(outer), ones(Int,ndims_out-length_outer))

        size_in = size(A)
        size_out = ntuple(i->inner[i]*size(A,i)*outer[i],ndims_out)::Dims
        inner_size_out = ntuple(i->inner[i]*size(A,i),ndims_out)::Dims

        indices_in = Array(Int, ndims_in)
        indices_out = Array(Int, ndims_out)

        length_out = prod(size_out)
        R = similar(A, size_out)

        for index_out in 1:length_out
            indices_out = ind2sub(size_out, index_out)
            for t in 1:ndims_in
                # "Project" outer repetitions into inner repetitions
                indices_in[t] = mod1(indices_out[t], inner_size_out[t])
                # Find inner repetitions using flooring division
                indices_in[t] = Base.fld1(indices_in[t], inner[t])
            end
            index_in = sub2ind(size_in, indices_in...)
            R[index_out] = A[index_in]
        end

        return R
    end
else
    const repeat = Base.repeat
end

if VERSION < v"0.5.0-dev+4267"
    if OS_NAME == :Windows
        const KERNEL = :NT
    else
        const KERNEL = OS_NAME
    end

    @eval is_apple()   = $(KERNEL == :Darwin)
    @eval is_linux()   = $(KERNEL == :Linux)
    @eval is_bsd()     = $(KERNEL in (:FreeBSD, :OpenBSD, :NetBSD, :Darwin, :Apple))
    @eval is_unix()    = $(is_linux() || is_bsd())
    @eval is_windows() = $(KERNEL == :NT)
    export is_apple, is_linux, is_bsd, is_unix, is_windows
else
    const KERNEL = Sys.KERNEL
end


if isdefined(Core, :String) && isdefined(Core, :AbstractString)
    # Not exported in order to not break code on 0.5
    typealias UTF8String Core.String
    typealias ASCIIString Core.String
else
    typealias String Base.ByteString
    @compat (::Type{Base.ByteString})(io::Base.AbstractIOBuffer) = bytestring(io)
    @compat (::Type{Base.ByteString})(io::IOBuffer) = bytestring(io)
    @compat (::Type{Base.ByteString})(s::Cstring) = bytestring(s)
    @compat (::Type{Base.ByteString})(v::Vector{UInt8}) = bytestring(v)
    @compat (::Type{Base.ByteString})(p::Union{Ptr{Int8},Ptr{UInt8}}) = bytestring(p)
    @compat (::Type{Base.ByteString})(p::Union{Ptr{Int8},Ptr{UInt8}}, len::Integer) = bytestring(p, len)
    @compat (::Type{Base.ByteString})(s::AbstractString) = bytestring(s)
end

if VERSION < v"0.5.0-dev+2285"
    fieldoffset(T, i) = @compat UInt(fieldoffsets(T)[i])
    export fieldoffset
end

if !isdefined(Base, :view)
    const view = slice
end

if !isdefined(Base, :pointer_to_string)

    function pointer_to_string(p::Ptr{UInt8}, len::Integer, own::Bool=false)
        a = ccall(:jl_ptr_to_array_1d, Vector{UInt8},
                  (Any, Ptr{UInt8}, Csize_t, Cint), Vector{UInt8}, p, len, own)
        ccall(:jl_array_to_string, String, (Any,), a)
    end

    pointer_to_string(p::Ptr{UInt8}, own::Bool=false) =
        pointer_to_string(p, ccall(:strlen, Csize_t, (Cstring,), p), own)

end

if VERSION < v"0.5.0-dev+4612"
    export unsafe_string, unsafe_wrap
    unsafe_wrap(::Type{Compat.String}, p::@compat(Union{Ptr{Int8},Ptr{UInt8}}), own::Bool=false) = pointer_to_string(convert(Ptr{UInt8}, p), own)
    unsafe_wrap(::Type{Compat.String}, p::@compat(Union{Ptr{Int8},Ptr{UInt8}}), len, own::Bool=false) = pointer_to_string(convert(Ptr{UInt8}, p), len, own)
    unsafe_wrap(::Type{Array}, p::Ptr, dims, own::Bool=false) = pointer_to_array(p, dims, own)
    unsafe_string(p::@compat(Union{Ptr{Int8},Ptr{UInt8}})) = bytestring(p)
    unsafe_string(p::@compat(Union{Ptr{Int8},Ptr{UInt8}}), len) = bytestring(p, len)
    if Cstring != Ptr{UInt8}
        unsafe_string(p::Cstring) = unsafe_string(Ptr{UInt8}(p))
    end
end

if !isdefined(Base, :allunique)
    allunique(itr) = length(itr) == length(unique(itr))
    export allunique
end

if !isdefined(Base, :promote_eltype_op)
    if isdefined(Base, :promote_op)
        promote_eltype_op(::Any) = Union{}
        promote_eltype_op{T}(op, ::AbstractArray{T}) = Base.promote_op(op, T)
        promote_eltype_op{T}(op, ::T               ) = Base.promote_op(op, T)
        promote_eltype_op{R,S}(op, ::AbstractArray{R}, ::AbstractArray{S}) = Base.promote_op(op, R, S)
        promote_eltype_op{R,S}(op, ::AbstractArray{R}, ::S) = Base.promote_op(op, R, S)
        promote_eltype_op{R,S}(op, ::R, ::AbstractArray{S}) = Base.promote_op(op, R, S)
        promote_eltype_op(op, A, B, C, D...) = Base.promote_op(op, eltype(A), promote_eltype_op(op, B, C, D...))
    else
        promote_eltype_op(::Any) = Union{}
        promote_eltype_op{T}(op, ::AbstractArray{T}) = T
        promote_eltype_op{T}(op, ::T               ) = T
        promote_eltype_op{R,S}(op, ::AbstractArray{R}, ::AbstractArray{S}) = Base.promote_type(R, S)
        promote_eltype_op{R,S}(op, ::AbstractArray{R}, ::S) = Base.promote_type(R, S)
        promote_eltype_op{R,S}(op, ::R, ::AbstractArray{S}) = Base.promote_type(R, S)
        promote_eltype_op(op, A, B, C, D...) = Base.promote_type(eltype(A), promote_eltype_op(op, B, C, D...))
    end
else
    import Base.promote_eltype_op
end

if VERSION < v"0.5.0-dev+4351"
    Base.join(io::IO, items) =
        print_joined(io, items)
    Base.join(io::IO, items, delim) =
        print_joined(io, items, delim)
    Base.join(io::IO, items, delim, last) =
        print_joined(io, items, delim, last)
    Base.unescape_string(io, s::AbstractString) =
        print_unescaped(io, s)
    Base.escape_string(io, str::AbstractString, esc::AbstractString) =
        print_escaped(io, str, esc)
end

if VERSION < v"0.5.0-dev+4340"
    Base.show(io::IO, mime, x) = writemime(io, mime, x)
end

if VERSION >= v"0.5.0-dev+4338"
    import Base.LinAlg.BLAS.@blasfunc
elseif VERSION >= v"0.5.0-dev+1871"
    import Base.@blasfunc
else
    macro blasfunc(x)
        Expr(:quote, Base.blasfunc(x))
    end
end

import Base: redirect_stdin, redirect_stdout, redirect_stderr
if VERSION < v"0.6.0-dev.374"
    for (F,S) in ((:redirect_stdin, :STDIN), (:redirect_stdout, :STDOUT), (:redirect_stderr, :STDERR))
        @eval function $F(f::Function, stream)
            STDOLD = $S
            $F(stream)
            try f() finally $F(STDOLD) end
        end
    end
end

end # module
