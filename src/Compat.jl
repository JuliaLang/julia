__precompile__()

module Compat

using Base.Meta

"""Get just the function part of a function declaration."""
withincurly(ex) = isexpr(ex, :curly) ? ex.args[1] : ex

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
if VERSION < v"0.6.0-dev.2043"
    Base.take!(t::Task) = consume(t)
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

function rewrite_dict(ex)
    length(ex.args) == 1 && return ex

    f = ex.args[1]
    if isexpr(f, :curly)
        newex = Expr(:typed_dict, :($(f.args[2])=>$(f.args[3])))
    else
        newex = Expr(:dict)
    end

    for i = 2:length(ex.args)
        pair = ex.args[i]
        !isexpr(pair, :(=>)) && return ex
        push!(newex.args, pair)
    end
    newex
end

function rewrite_ordereddict(ex)
    length(ex.args) == 1 && return ex

    f = ex.args[1]
    newex = Expr(:call, f, :[])

    for i = 2:length(ex.args)
        pair = ex.args[i]
        !isexpr(pair, :(=>)) && return ex
        push!(newex.args[2].args, Expr(:tuple, pair.args...))
    end

    newex
end

# rewrite Julia 0.4-style split or rsplit (str, splitter; kws...)
# into 0.2/0.3-style positional arguments
function rewrite_split(ex, f)
    limit = nothing
    keep = nothing
    for i in 4:length(ex.args)
        if isexpr(ex.args[i], :kw)
            kw = ex.args[i].args
            if kw[1] == :limit
                limit = kw[2]
            elseif kw[1] == :keep
                keep = kw[2]
            end
        end
    end
    if limit == nothing
        if keep == nothing
            return Expr(:call, f, ex.args[2], ex.args[3])
        else
            return Expr(:call, f, ex.args[2], ex.args[3], keep)
        end
    else
        if keep == nothing
            return Expr(:call, f, ex.args[2], ex.args[3], limit)
        else
            return Expr(:call, f, ex.args[2], ex.args[3], limit, keep)
        end
    end
end

# rewrites all subexpressions of the form `a => b` to `(a, b)`
function rewrite_pairs_to_tuples!(expr::Expr)
    if expr.head == :(=>)
        expr.head = :tuple
    end
    for subexpr in expr.args
        isa(subexpr, Expr) && rewrite_pairs_to_tuples!(subexpr)
    end
    return expr
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

is_index_style(ex::Expr) = ex == :(Compat.IndexStyle) || ex == :(Base.IndexStyle) ||
    (ex.head == :(.) && (ex.args[1] == :Compat || ex.args[1] == :Base) &&
         ex.args[2] == Expr(:quote, :IndexStyle))

is_index_style(arg) = false

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

# JuliaLang/julia#10543
if !isdefined(Base, :tryparse)
    function tryparse{T}(::Type{T}, args...)
        try
            Nullable(Base.parse(T, args...))
        catch
            Nullable{T}()
        end
    end
end

import Base.unsafe_convert

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

if VERSION < v"0.5.0-dev+4002"
    @inline broadcast_getindex(arg, idx) = arg[(idx - 1) % length(arg) + 1]
    # Optimize for single element
    @inline broadcast_getindex(arg::Number, idx) = arg
    @inline broadcast_getindex{T}(arg::Array{T,0}, idx) = arg[1]

    # If we know from syntax level that we don't need wrapping
    @inline broadcast_getindex_naive(arg, idx) = arg[idx]
    @inline broadcast_getindex_naive(arg::Number, idx) = arg
    @inline broadcast_getindex_naive{T}(arg::Array{T,0}, idx) = arg[1]

    # For vararg support
    @inline getindex_vararg(idx) = ()
    @inline getindex_vararg(idx, arg1) = (broadcast_getindex(arg1, idx),)
    @inline getindex_vararg(idx, arg1, arg2) =
        (broadcast_getindex(arg1, idx), broadcast_getindex(arg2, idx))
    @inline getindex_vararg(idx, arg1, arg2, arg3, args...) =
        (broadcast_getindex(arg1, idx), broadcast_getindex(arg2, idx),
         broadcast_getindex(arg3, idx), getindex_vararg(idx, args...)...)

    @inline getindex_naive_vararg(idx) = ()
    @inline getindex_naive_vararg(idx, arg1) =
        (broadcast_getindex_naive(arg1, idx),)
    @inline getindex_naive_vararg(idx, arg1, arg2) =
        (broadcast_getindex_naive(arg1, idx),
         broadcast_getindex_naive(arg2, idx))
    @inline getindex_naive_vararg(idx, arg1, arg2, arg3, args...) =
        (broadcast_getindex_naive(arg1, idx),
         broadcast_getindex_naive(arg2, idx),
         broadcast_getindex_naive(arg3, idx),
         getindex_naive_vararg(idx, args...)...)

    # Decide if the result should be scalar or array
    # `size() === ()` is not good enough since broadcasting on
    # a scalar should return a scalar where as broadcasting on a 0-dim
    # array should return a 0-dim array.
    @inline should_return_array(::Val{true}, args...) = Val{true}()
    @inline should_return_array(::Val{false}) = Val{false}()
    @inline should_return_array(::Val{false}, arg1) = Val{false}()
    @inline should_return_array(::Val{false}, arg1::AbstractArray) = Val{true}()
    @inline should_return_array(::Val{false}, arg1::AbstractArray,
                                arg2::AbstractArray) = Val{true}()
    @inline should_return_array(::Val{false}, arg1,
                                arg2::AbstractArray) = Val{true}()
    @inline should_return_array(::Val{false}, arg1::AbstractArray,
                                arg2) = Val{true}()
    @inline should_return_array(::Val{false}, arg1, arg2) = Val{false}()
    @inline should_return_array(::Val{false}, arg1, arg2, args...) =
        should_return_array(should_return_array(Val{false}(), arg1, arg2),
                            args...)

    @inline broadcast_return(res1d, shp, ret_ary::Val{false}) = res1d[1]
    @inline broadcast_return(res1d, shp, ret_ary::Val{true}) = reshape(res1d, shp)

    @inline need_full_getindex(shp) = false
    @inline need_full_getindex(shp, arg1::Number) = false
    @inline need_full_getindex{T}(shp, arg1::Array{T,0}) = false
    @inline need_full_getindex(shp, arg1) = shp != size(arg1)
    @inline need_full_getindex(shp, arg1, arg2) =
        need_full_getindex(shp, arg1) || need_full_getindex(shp, arg2)
    @inline need_full_getindex(shp, arg1, arg2, arg3, args...) =
        need_full_getindex(shp, arg1, arg2) || need_full_getindex(shp, arg3) ||
        need_full_getindex(shp, args...)

    function rewrite_broadcast(f, args)
        nargs = length(args)
        # This actually allows multiple splatting...,
        # which is now allowed on master.
        # The previous version that simply calls broadcast so removing that
        # will be breaking. Oh, well....
        is_vararg = Bool[isexpr(args[i], :...) for i in 1:nargs]
        names = [gensym("broadcast") for i in 1:nargs]
        new_args = [is_vararg[i] ? Expr(:..., names[i]) : names[i]
                    for i in 1:nargs]
        # Optimize for common case where we know the index doesn't need
        # any wrapping
        naive_getidx_for = function (i, idxvar)
            if is_vararg[i]
                Expr(:..., :($Compat.getindex_naive_vararg($idxvar,
                                                           $(names[i])...)))
            else
                :($Compat.broadcast_getindex_naive($(names[i]), $idxvar))
            end
        end
        always_naive = nargs == 1 && !is_vararg[1]
        getidx_for = if always_naive
            naive_getidx_for
        else
            function (i, idxvar)
                if is_vararg[i]
                    Expr(:..., :($Compat.getindex_vararg($idxvar,
                                                         $(names[i])...)))
                else
                    :($Compat.broadcast_getindex($(names[i]), $idxvar))
                end
            end
        end
        @gensym allidx
        @gensym newshape
        @gensym res1d
        @gensym idx
        @gensym ret_ary

        res1d_expr = quote
            $res1d = [$f($([naive_getidx_for(i, idx) for i in 1:nargs]...))
                      for $idx in $allidx]
        end
        if !always_naive
            res1d_expr = quote
                if $Compat.need_full_getindex($newshape, $(new_args...))
                    $res1d = [$f($([getidx_for(i, idx) for i in 1:nargs]...))
                              for $idx in $allidx]
                else
                    $res1d_expr
                end
            end
        end

        return quote
            # The `local` makes sure type inference can infer the type even
            # in global scope as long as the input is type stable
            local $(names...)
            $([:($(names[i]) = $(is_vararg[i] ? args[i].args[1] : args[i]))
               for i in 1:nargs]...)
            local $newshape = $(Base.Broadcast).broadcast_shape($(new_args...))
            # `eachindex` is not generic enough
            local $allidx = 1:prod($newshape)
            local $ret_ary = $Compat.should_return_array(Val{false}(),
                                                         $(new_args...))
            local $res1d
            $res1d_expr
            $Compat.broadcast_return($res1d, $newshape, $ret_ary)
        end
    end
end

if VERSION < v"0.6.0-dev.2782"
    function new_style_typealias(ex::ANY)
        isexpr(ex, :(=)) || return false
        ex = ex::Expr
        return length(ex.args) == 2 && isexpr(ex.args[1], :curly)
    end
else
    new_style_typealias(ex) = false
end

function _compat(ex::Expr)
    if ex.head === :call
        f = ex.args[1]
        if VERSION < v"0.5.0-dev+4340" && length(ex.args) > 3 &&
                istopsymbol(withincurly(ex.args[1]), :Base, :show)
            ex = rewrite_show(ex)
        elseif VERSION < v"0.6.0-dev.826" && length(ex.args) == 3 && # julia#18510
                istopsymbol(withincurly(ex.args[1]), :Base, :Nullable)
            ex = Expr(:call, f, ex.args[2], Expr(:call, :(Compat._Nullable_field2), ex.args[3]))
        end
        if VERSION < v"0.5.0-dev+4305"
            rewrite_iocontext!(ex)
        end
    elseif ex.head === :curly
        f = ex.args[1]
        if ex == :(Ptr{Void})
            # Do not change Ptr{Void} to Ptr{Nothing}: 0.4.0-dev+768
            return ex
        elseif VERSION < v"0.6.0-dev.2575" #20414
            ex = Expr(:curly, map(a -> isexpr(a, :call, 2) && a.args[1] == :(<:) ?
                                  :($TypeVar($(QuoteNode(gensym(:T))), $(a.args[2]), false)) : a,
                                  ex.args)...)
        end
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
            return rewrite_broadcast(_compat(ex.args[1]),
                                     map(_compat, ex.args[2].args))
        elseif !isa(ex.args[2], QuoteNode) &&
               !(isexpr(ex.args[2], :quote) && isa(ex.args[2].args[1], Symbol))
            # f.(arg) -> broadcast(f, arg)
            return rewrite_broadcast(_compat(ex.args[1]), [_compat(ex.args[2])])
        end
    elseif new_style_typealias(ex)
        ex.head = :typealias
    elseif ex.head === :const && length(ex.args) == 1 && new_style_typealias(ex.args[1])
        ex = ex.args[1]::Expr
        ex.head = :typealias
    elseif ex.head === :import
        if VERSION < v"0.5.0-dev+4340" && length(ex.args) == 2 && ex.args[1] === :Base && ex.args[2] === :show
            return quote
                import Base.show
                import Base.writemime
            end
        end
    elseif VERSION < v"0.5.0-dev+5575" #17510
        if isexpr(ex, :comparison)
            if length(ex.args) == 3 && ex.args[2] == :.=
                return :(Base.broadcast!(Base.identity, $(_compat(todotview(ex.args[1]))), $(_compat(ex.args[3]))))
            elseif length(ex.args) > 3 && ex.args[2] == :.=
                return :(Base.broadcast!(Base.identity, $(_compat(todotview(ex.args[1]))), $(_compat(Expr(:comparison, ex.args[3:end]...)))))
            end
        elseif ex.head == :.= && length(ex.args) == 2 # may arise in macro-constructed expressions
            return :(Base.broadcast!(Base.identity, $(_compat(todotview(ex.args[1]))), $(_compat(ex.args[2]))))
        end
    end
    if VERSION < v"0.5.0-dev+5575" #17510
        # transform e.g. x .+= y into x .= x .+ y:
        shead = string(ex.head)
        if first(shead) == '.' && length(shead) > 2 && last(shead) == '=' && length(ex.args) == 2
            return _compat(Expr(:comparison, ex.args[1], :.=, Expr(:call, Symbol(shead[1:end-1]), ex.args...)))
        end
    end
    if VERSION < v"0.6.0-dev.2840"
        if ex.head == :(=) && isa(ex.args[1], Expr) && ex.args[1].head == :call
            a = ex.args[1].args[1]
            if is_index_style(a)
                ex.args[1].args[1] = :(Base.linearindexing)
            elseif isa(a, Expr) && a.head == :curly
                if is_index_style(a.args[1])
                    ex.args[1].args[1].args[1] = :(Base.linearindexing)
                end
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

function _get_typebody(ex::Expr)
    args = ex.args
    if ex.head !== :type || length(args) != 3 || args[1] !== true
        throw(ArgumentError("Invalid usage of @compat: $ex"))
    end
    name = args[2]
    if !isexpr(args[3], :block)
        throw(ArgumentError("Invalid type declaration: $ex"))
    end
    body = (args[3]::Expr).args
    filter!(body) do e
        if isa(e, LineNumberNode) || isexpr(e, :line)
            return false
        end
        return true
    end
    return name, body
end

function _compat_primitive(typedecl)
    name, body = _get_typebody(typedecl)
    if length(body) != 1
        throw(ArgumentError("Invalid primitive type declaration: $typedecl"))
    end
    return Expr(:bitstype, body[1], name)
end

function _compat_abstract(typedecl)
    name, body = _get_typebody(typedecl)
    if length(body) != 0
        throw(ArgumentError("Invalid abstract type declaration: $typedecl"))
    end
    return Expr(:abstract, name)
end

macro compat(ex...)
    if VERSION < v"0.6.0-dev.2746" && length(ex) == 2 && ex[1] === :primitive
        return esc(_compat_primitive(ex[2]))
    elseif length(ex) != 1
        throw(ArgumentError("@compat called with wrong number of arguments: $ex"))
    elseif (VERSION < v"0.6.0-dev.2746" && isexpr(ex[1], :abstract) &&
            length(ex[1].args) == 1 && isexpr(ex[1].args[1], :type))
        # This can in principle be handled in nested case but we do not
        # do that to be consistent with primitive types.
        return esc(_compat_abstract(ex[1].args[1]))
    end
    esc(_compat(ex[1]))
end

export @compat, @inline, @noinline

import Base.@irrational

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

function primarytype(t::ANY)
    tn = t.name
    if isdefined(tn, :primary)
        return tn.primary
    else
        return tn.wrapper
    end
end

export @functorize
macro functorize(f)
    if VERSION >= v"0.5.0-dev+3701"
        f === :scalarmax       ? :(Base.scalarmax) :
        f === :scalarmin       ? :(Base.scalarmin) :
        f === :centralizedabs2fun ? :(primarytype(typeof(Base.centralizedabs2fun(0)))) :
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
            f
        if VERSION >= v"0.4.0-dev+4902"
            f = f === :<           ? :(Base.LessFun()) :
                f === :>           ? :(Base.MoreFun()) :
                f
        end
        if VERSION >= v"0.4.0-dev+4902"
            f = f === :conj        ? :(Base.ConjFun()) :
                f
        end
        if VERSION >= v"0.4.0-dev+6254"
            f = f === :-           ? :(Base.SubFun()) :
                f === :^           ? :(Base.PowFun()) :
                f
        end
        if VERSION >= v"0.4.0-dev+6256"
            f = f === :/           ? :(Base.RDivFun()) :
                f === :\           ? :(Base.LDivFun()) :
                f === :div         ? :(Base.IDivFun()) :
                f
        end
        if VERSION >= v"0.4.0-dev+6353"
            f = f === :$           ? :(Base.XorFun()) :
                f === :.+          ? :(Base.DotAddFun()) :
                f === :.-          ? :(Base.DotSubFun()) :
                f === :.*          ? :(Base.DotMulFun()) :
                f === :mod         ? :(Base.ModFun()) :
                f === :rem         ? :(Base.RemFun()) :
                # DotRemFun is defined, but ::call(::DotRemFun, ...) is not until later
                #f === :.%          ? :(Base.DotRemFun()) :
                f === :.<<         ? :(Base.DotLSFun()) :
                f === :.>>         ? :(Base.DotRSFun()) :
                f
        end
        if VERSION >= v"0.4.0-dev+6359"
            f = f === :./          ? :(Base.DotRDivFun()) :
                f
        end
        if VERSION >= v"0.4.0-rc1+59"
            f = f === :max         ? :(Base.ElementwiseMaxFun()) :
                f === :min         ? :(Base.ElementwiseMinFun()) :
                f
        end
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

import Base: srand, rand, rand!

if isdefined(Core, :String) && isdefined(Core, :AbstractString)
    # Not exported in order to not break code on 0.5
    const UTF8String = Core.String
    const ASCIIString = Core.String
else
    const String = Base.ByteString
    if VERSION >= v"0.4.0-dev+5243"
        @compat (::Type{Base.ByteString})(io::Base.AbstractIOBuffer) = bytestring(io)
    elseif VERSION >= v"0.4.0-dev+1246"
        @compat (::Type{Base.ByteString})(io::IOBuffer) = bytestring(io)
    end
    if VERSION >= v"0.4.0-dev+1246"
        @compat (::Type{Base.ByteString})(s::Cstring) = bytestring(s)
        @compat (::Type{Base.ByteString})(v::Vector{UInt8}) = bytestring(v)
        @compat (::Type{Base.ByteString})(p::Union{Ptr{Int8},Ptr{UInt8}}) = bytestring(p)
        @compat (::Type{Base.ByteString})(p::Union{Ptr{Int8},Ptr{UInt8}}, len::Integer) = bytestring(p, len)
        @compat (::Type{Base.ByteString})(s::AbstractString) = bytestring(s)
    end
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

if VERSION < v"0.6.0-dev.528"
    macro __DIR__()
        Base.source_dir()
    end
    export @__DIR__
end

# PR #17302
# Provide a non-deprecated version of `@vectorize_(1|2)arg` macro which defines
# deprecated version of the function so that the depwarns can be fixed without
# breaking users.
# Packages are expected to use this to maintain the old API until all users
# of the deprecated vectorized function have migrated.
# These macros should raise a depwarn when the `0.5` support is dropped from
# `Compat` and be dropped when the support for `0.6` is dropped from `Compat`.
# Modified based on the version copied from 0.6 Base.
macro dep_vectorize_1arg(S, f)
    S = esc(S)
    f = esc(f)
    T = esc(:T)
    x = esc(:x)
    AbsArr = esc(:AbstractArray)
    ## Depwarn to be enabled when 0.5 support is dropped.
    # depwarn("Implicit vectorized function is deprecated in favor of compact broadcast syntax.",
    #         Symbol("@dep_vectorize_1arg"))
    :(@deprecate $f{$T<:$S}($x::$AbsArr{$T}) @compat($f.($x)))
end

macro dep_vectorize_2arg(S, f)
    S = esc(S)
    f = esc(f)
    T1 = esc(:T1)
    T2 = esc(:T2)
    x = esc(:x)
    y = esc(:y)
    AbsArr = esc(:AbstractArray)
    ## Depwarn to be enabled when 0.5 support is dropped.
    # depwarn("Implicit vectorized function is deprecated in favor of compact broadcast syntax.",
    #         Symbol("@dep_vectorize_2arg"))
    quote
        @deprecate $f{$T1<:$S}($x::$S, $y::$AbsArr{$T1}) @compat($f.($x,$y))
        @deprecate $f{$T1<:$S}($x::$AbsArr{$T1}, $y::$S) @compat($f.($x,$y))
        @deprecate $f{$T1<:$S,$T2<:$S}($x::$AbsArr{$T1}, $y::$AbsArr{$T2}) @compat($f.($x,$y))
    end
end

if VERSION < v"0.5.0-dev+4677"
    using Base.LinAlg: HermOrSym
    Base.chol(A::HermOrSym) = Base.LinAlg.chol!(A.uplo == 'U' ? copy(A.data) : A.data')
    Base.cholfact(A::HermOrSym) = cholfact(A.data, Symbol(A.uplo))
    Base.cholfact!(A::HermOrSym) = cholfact!(A.data, Symbol(A.uplo))

    Base.cholfact(A::HermOrSym, T::Type) = cholfact(A.data, Symbol(A.uplo), T)
    Base.cholfact!(A::HermOrSym, T::Type) = cholfact!(A.data, Symbol(A.uplo), T)
end

# broadcast over same length tuples, from julia#16986
if v"0.5.0-dev+4002" ≤ VERSION < v"0.6.0-dev.693"
    Base.Broadcast.broadcast{N}(f, t::NTuple{N}, ts::Vararg{NTuple{N}}) = map(f, t, ts...)
end

# julia#18510
if VERSION < v"0.6.0-dev.826"
    _Nullable_field2(x) = !x
else
    _Nullable_field2(x) = x
end

# julia#18484
if VERSION < v"0.6.0-dev.848"
    unsafe_get(x::Nullable) = x.value
    unsafe_get(x) = x
    export unsafe_get
    Base.isnull(x) = false
end

# julia#18977
if !isdefined(Base, :xor)
    const xor = $
    const ⊻ = xor
    export xor, ⊻
end

# julia#19246
if !isdefined(Base, :numerator)
    const numerator = num
    const denominator = den
    export numerator, denominator
end

# julia #19950
if !isdefined(Base, :iszero)
    iszero(x) = x == zero(x)
    iszero(x::Number) = x == 0
    iszero(x::AbstractArray) = all(iszero, x)
    export iszero
end

# julia#19088
if VERSION < v"0.6.0-dev.1256"
    Base.take!(io::Base.AbstractIOBuffer) = takebuf_array(io)
end

# julia #17323
if VERSION < v"0.5.0-dev+5380"
    export transcode
    transcode{T<:Compat.String}(::Type{T}, src::Union{Compat.String,Vector{UInt8}}) = utf8(src)
    transcode{T<:Compat.String}(::Type{T}, src::Vector{UInt16}) = utf8(utf16(src))
    transcode{T<:Compat.String}(::Type{T}, src::Vector{UInt32}) = utf8(utf32(src))
    transcode(::Type{UInt8}, src::Vector{UInt8}) = src
    transcode(::Type{UInt8}, src) = transcode(Compat.String, src).data
    function transcode(::Type{UInt16}, src::Union{Compat.String,Vector{UInt8}})
        d = utf16(utf8(src)).data
        return resize!(d, length(d)-1) # strip off trailing NUL codeunit
    end
    function transcode(::Type{UInt32}, src::Union{Compat.String,Vector{UInt8}})
        d = utf32(utf8(src)).data
        return resize!(d, length(d)-1) # strip off trailing NUL codeunit
    end
    transcode(::Type{UInt16}, src::Vector{UInt16}) = src
    transcode(::Type{UInt32}, src::Vector{UInt32}) = src
    if Cwchar_t == Int32
        transcode(::Type{Cwchar_t}, src::Vector{Cwchar_t}) = src
        transcode(::Type{Cwchar_t}, src) = reinterpret(Cwchar_t, transcode(UInt32, src))
        transcode(::Type{UInt8}, src::Vector{Cwchar_t}) = transcode(UInt8, reinterpret(UInt32, src))
        transcode(T, src::Vector{Cwchar_t}) = transcode(T, reinterpret(UInt32, src))
    end
end

# julia #17155 function composition and negation
if VERSION < v"0.6.0-dev.1883"
    export ∘
    ∘(f, g) = (x...)->f(g(x...))
    @compat Base.:!(f::Function) = (x...)->!f(x...)
end

if VERSION < v"0.5.0-dev+1450"
    Base.Diagonal(v::AbstractVector) = Diagonal(collect(v))
end
if VERSION < v"0.5.0-dev+3669"
    using Base: promote_op
    import Base: *
    eval(Expr(:typealias, :(SubMatrix{T}), :(SubArray{T,2})))
    (*)(A::SubMatrix, D::Diagonal) =
        scale!(similar(A, promote_op(*, eltype(A), eltype(D.diag))), A, D.diag)
    (*)(D::Diagonal, A::SubMatrix) =
        scale!(similar(A, promote_op(*, eltype(A), eltype(D.diag))), D.diag, A)
end

if VERSION >= v"0.5.0-dev+5509" && VERSION < v"0.6.0-dev.1632"
    # To work around unsupported syntax on Julia 0.4
    include_string("export .&, .|")
    include_string(".&(xs...) = broadcast(&, xs...)")
    include_string(".|(xs...) = broadcast(|, xs...)")
end

if VERSION < v"0.6.0-dev.2093" # Compat.isapprox to allow for NaNs
    using Base.rtoldefault
    function isapprox(x::Number, y::Number; rtol::Real=rtoldefault(x,y), atol::Real=0, nans::Bool=false)
        x == y || (isfinite(x) && isfinite(y) && abs(x-y) <= atol + rtol*max(abs(x), abs(y))) || (nans && isnan(x) && isnan(y))
    end
else
    import Base.isapprox
end

# julia #13998 single-argument min and max
if VERSION < v"0.5.0-dev+1279"
    Base.min(x::Real) = x
    Base.max(x::Real) = x
    Base.minmax(x::Real) = (x, x)
end

module TypeUtils
    using ..Compat: @static
    @static if isdefined(Base, :isabstract)
        using Base: isabstract, parameter_upper_bound, typename
    else
        isabstract(t::DataType) = t.abstract
        if isdefined(Base, :TypeConstructor)
            isabstract(t::TypeConstructor) = isabstract(t.body)
        end
        isabstract(t::ANY) = false
        parameter_upper_bound(t::DataType, idx) = t.parameters[idx].ub
        typename(t::DataType) = t.name
    end
    export isabstract, parameter_upper_bound, typename
end # module TypeUtils

# @view, @views, @__dot__
include("arraymacros.jl")

# julia #18839
if VERSION < v"0.6.0-dev.1024"
    @eval module Iterators
        export countfrom, cycle, drop, enumerate, flatten, product, repeated,
               rest, take, zip, partition

        import Base: eltype, start, next, done, length, size, ndims
        using Base: tuple_type_cons
        using Base: countfrom, cycle, drop, enumerate, repeated, rest, take,
                    zip
        using Compat

        # julia #14805
        if VERSION < v"0.5.0-dev+3256"
            immutable Flatten{I}
                it::I
            end

            flatten(itr) = Flatten(itr)

            eltype{I}(::Type{Flatten{I}}) = eltype(eltype(I))

            function start(f::Flatten)
                local inner, s2
                s = start(f.it)
                d = done(f.it, s)
                # this is a simple way to make this function type stable
                d && throw(ArgumentError("argument to Flatten must contain at least one iterator"))
                while !d
                    inner, s = next(f.it, s)
                    s2 = start(inner)
                    !done(inner, s2) && break
                    d = done(f.it, s)
                end
                return s, inner, s2
            end

            @inline function next(f::Flatten, state)
                s, inner, s2 = state
                val, s2 = next(inner, s2)
                while done(inner, s2) && !done(f.it, s)
                    inner, s = next(f.it, s)
                    s2 = start(inner)
                end
                return val, (s, inner, s2)
            end

            @inline function done(f::Flatten, state)
                s, inner, s2 = state
                return done(f.it, s) && done(inner, s2)
            end
        else
            using Base: flatten
        end

        # julia #14596
        if VERSION < v"0.5.0-dev+2305"
            # Product -- cartesian product of iterators
            @compat abstract type AbstractProdIterator end

            immutable Prod2{I1, I2} <: AbstractProdIterator
                a::I1
                b::I2
            end

            product(a) = Zip1(a)
            product(a, b) = Prod2(a, b)
            eltype{I1,I2}(::Type{Prod2{I1,I2}}) = Tuple{eltype(I1), eltype(I2)}
            length(p::AbstractProdIterator) = length(p.a)*length(p.b)

            function start(p::AbstractProdIterator)
                s1, s2 = start(p.a), start(p.b)
                s1, s2, Nullable{eltype(p.b)}(), (done(p.a,s1) || done(p.b,s2))
            end

            @inline function prod_next(p, st)
                s1, s2 = st[1], st[2]
                v1, s1 = next(p.a, s1)

                nv2 = st[3]
                if isnull(nv2)
                    v2, s2 = next(p.b, s2)
                else
                    v2 = nv2.value
                end

                if done(p.a, s1)
                    return (v1,v2), (start(p.a), s2, oftype(nv2,nothing), done(p.b,s2))
                end
                return (v1,v2), (s1, s2, Nullable(v2), false)
            end

            @inline next(p::Prod2, st) = prod_next(p, st)
            @inline done(p::AbstractProdIterator, st) = st[4]

            immutable Prod{I1, I2<:AbstractProdIterator} <: AbstractProdIterator
                a::I1
                b::I2
            end

            product(a, b, c...) = Prod(a, product(b, c...))
            eltype{I1,I2}(::Type{Prod{I1,I2}}) = tuple_type_cons(eltype(I1), eltype(I2))

            @inline function next{I1,I2}(p::Prod{I1,I2}, st)
                x = prod_next(p, st)
                ((x[1][1],x[1][2]...), x[2])
            end
        else
            using Base: product
        end

        # julia #15409
        if VERSION < v"0.5.0-dev+3510"
            partition{T}(c::T, n::Int) = PartitionIterator{T}(c, n)

            type PartitionIterator{T}
                c::T
                n::Int
            end

            eltype{T}(::Type{PartitionIterator{T}}) = Vector{eltype(T)}

            function length(itr::PartitionIterator)
                l = length(itr.c)
                return div(l, itr.n) + ((mod(l, itr.n) > 0) ? 1 : 0)
            end

            start(itr::PartitionIterator) = start(itr.c)

            done(itr::PartitionIterator, state) = done(itr.c, state)

            function next{T<:Vector}(itr::PartitionIterator{T}, state)
                l = state
                r = min(state + itr.n-1, length(itr.c))
                return sub(itr.c, l:r), r + 1
            end

            function next(itr::PartitionIterator, state)
                v = Vector{eltype(itr.c)}(itr.n)
                i = 0
                while !done(itr.c, state) && i < itr.n
                    i += 1
                    v[i], state = next(itr.c, state)
                end
                return resize!(v, i), state
            end
        else
            using Base: partition
        end
    end
else
    using Base: Iterators
end

if VERSION < v"0.6.0-dev.2840"
    export IndexStyle, IndexLinear, IndexCartesian
    eval(Expr(:typealias, :IndexStyle, :(Base.LinearIndexing)))
    eval(Expr(:typealias, :IndexLinear, :(Base.LinearFast)))
    eval(Expr(:typealias, :IndexCartesian, :(Base.LinearSlow)))
    IndexStyle{T}(::Type{T}) = Base.linearindexing(T)
    IndexStyle(args...) = Base.linearindexing(args...)
end

include("to-be-deprecated.jl")

end # module Compat
