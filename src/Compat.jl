VERSION >= v"0.4.0-dev+6521" && __precompile__()

module Compat

using Base.Meta

if VERSION < v"0.4.0-dev+1419"
    export UInt, UInt8, UInt16, UInt32, UInt64, UInt128
    const UInt = Uint
    const UInt8 = Uint8
    const UInt16 = Uint16
    const UInt32 = Uint32
    const UInt64 = Uint64
    const UInt128 = Uint128
end

if VERSION < v"0.4.0-dev+4603"
    # used for C string arguments to ccall
    # (in Julia 0.4, these types also check for embedded NUL chars)
    const Cstring = Ptr{UInt8}
    const Cwstring = Ptr{Cwchar_t}
    export Cstring, Cwstring
end

if VERSION < v"0.4.0-dev+2340"
    const base64encode = base64
    export base64encode
end

if VERSION >= v"0.4.0-dev+3184"
    include("ngenerate.jl")
    using .CompatCartesian
    export @ngenerate, @nsplat
end

if VERSION < v"0.4.0-dev+1624"
    eachindex(A::AbstractArray) = 1:length(A)
    eachindex(a::Associative) = Base.KeyIterator(a)
    export eachindex
end

if VERSION < v"0.4.0-dev+1387"
    typealias AbstractString Base.String
    export AbstractString
end

if VERSION < v"0.4.0-dev+3324"
    typealias AssertionError ErrorException
    export AssertionError
end

if VERSION < v"0.4.0-dev+412"
    eval(Base, :(const IPAddr = IpAddr))
end

if VERSION < v"0.4.0-dev+2197"
    Base.IPv4(ipstr::AbstractString) = Base.parseipv4(ipstr)
    Base.IPv6(ipstr::AbstractString) = Base.parseipv6(ipstr)
end

if VERSION < v"0.4.0-dev+2200"
    function Base.isless{T<:Base.IPAddr}(a::T, b::T)
        return isless(a.host, b.host)
    end
end

if VERSION < v"0.4.0-dev+980"
    macro Dict(pairs...)
        esc(Expr(:dict, pairs...))
    end
    macro AnyDict(pairs...)
        esc(Expr(:typed_dict, :(Any=>Any), pairs...))
    end

    Base.Dict(kv) = dict_with_eltype(kv, eltype(kv))
    dict_with_eltype{K,V}(kv, ::Type{(K,V)}) = Dict{K,V}(kv)
    dict_with_eltype(kv, t) = Dict{Any,Any}(kv)
else
    macro Dict(pairs...)
        esc(Expr(:call, :Dict, pairs...))
    end
    macro AnyDict(pairs...)
        esc(Expr(:call, :(Base.AnyDict), pairs...))
    end
end

import Base: round, ceil, floor, trunc
if VERSION < v"0.4.0-dev+1827"
    for (fnew,fold) in ((:round, :iround), (:ceil, :iceil), (:floor, :ifloor), (:trunc, :itrunc))
        @eval begin
            ($fnew){T<:Integer}(::Type{T}, x::Integer) = convert(T, x)  # ambiguity resolution with digits/base version, not all old methods defined
            ($fnew){T<:Integer}(::Type{T}, x::Real) = ($fold)(T, x)
            ($fnew){T<:Integer}(::Type{T}, x::Rational) = convert(T, ($fold)(x)) # no e.g. iround(::Type{T}, x::Rational) is defined in 0.3
            function ($fnew){T,R}(::Type{T}, x::AbstractArray{R,1})
                [ ($fnew)(T, x[i])::T for i = 1:length(x) ]
            end
            function ($fnew){T,R}(::Type{T}, x::AbstractArray{R,2})
                [ ($fnew)(T, x[i,j])::T for i = 1:size(x,1), j = 1:size(x,2) ]
            end
            function ($fnew){T}(::Type{T}, x::AbstractArray)
                reshape([ ($fnew)(T, x[i])::T for i = 1:length(x) ], size(x))
            end
        end
    end
end

if VERSION < v"0.4.0-dev+838"
    Base.rem{T<:Integer}(n::Integer, ::Type{T}) = convert(T, n)
elseif VERSION < v"0.4.0-dev+1037"
    Base.rem{T<:Integer}(n::Integer, ::Type{T}) = itrunc(T, n)
elseif VERSION < v"0.4.0-dev+1039"
    Base.rem{T<:Integer}(n::Integer, ::Type{T}) = mod(n, T)
end

if VERSION < v"0.4.0-dev+2014"
    sizehint! = Base.sizehint
    export sizehint!
end

if VERSION < v"0.4.0-dev+3413"
    # based on pipe in base/process.jl:
    import Base: AbstractCmd, Redirectable
    function pipeline(cmd::AbstractCmd; stdin=nothing, stdout=nothing, stderr=nothing, append::Bool=false)
        if append && stdout === nothing && stderr === nothing
            error("append set to true, but no output redirections specified")
        end
        if stdin !== nothing
            cmd = stdin |> cmd
        end
        if stdout !== nothing
            cmd = append ? cmd >> stdout : cmd |> stdout
        end
        if stderr !== nothing
            cmd = append ? cmd .>> stderr : cmd .> stderr
        end
        return cmd
    end
    pipeline(cmd::AbstractCmd, dest) = pipeline(cmd, stdout=dest)
    pipeline(src::Union(Redirectable,AbstractString), cmd::AbstractCmd) = pipeline(cmd, stdin=src)
    pipeline(a, b, c, d...) = pipeline(pipeline(a,b), c, d...)
    export pipeline
elseif VERSION < v"0.4.0-dev+6987"
    const pipeline = Base.pipe
    export pipeline
end

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
    argtypes = ex.args[2:end]
    Expr(:call, :(Base.writemime), argtypes...)
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

if VERSION < v"0.4.0-dev+707"
    macro inline(ex)
        esc(ex)
    end
end

if VERSION < v"0.4.0-dev+2056"
    macro noinline(ex)
        esc(ex)
    end
end

if VERSION < v"0.4.0-dev+2440"
    bitrand(r::AbstractRNG, dims::Dims)   = rand!(r, BitArray(dims))
    bitrand(r::AbstractRNG, dims::Int...) = rand!(r, BitArray(dims))
    bitrand(dims::Dims)   = rand!(BitArray(dims))
    bitrand(dims::Int...) = rand!(BitArray(dims))
    export bitrand
    Base.rand(::Type{Bool}) = randbool()
end

if VERSION < v"0.4.0-dev+2485"
    startswith = Base.beginswith
    export startswith
end

if VERSION < v"0.4.0-dev+3609"
    fieldnames(x::DataType) = collect(x.names)
    function fieldnames(v)
        t = typeof(v)
        if !isa(t, DataType)
            throw(ArgumentError("cannot call fieldnames() on a non-composite type"))
        end
        return fieldnames(t)
    end
    export fieldnames
end

if VERSION < v"0.4.0-dev+3874"
    Base.parse{T<:Integer}(::Type{T}, c::Char) = parseint(T, c)
    Base.parse{T<:Integer}(::Type{T}, c::Char, base::Integer) = parseint(T, c, base)
    Base.parse{T<:Integer}(::Type{T}, s::AbstractString) = parseint(T, s)
    Base.parse{T<:Integer}(::Type{T}, s::AbstractString, base::Integer) = parseint(T, s, base)
    Base.parse{T<:Union(Float32,Float64)}(::Type{T}, s::AbstractString) = parsefloat(T, s)
end

if VERSION < v"0.4.0-dev+4539"
    Base.parse(::Type{BigFloat}, s::AbstractString) = BigFloat(s)
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

if VERSION < v"0.4.0-dev+3710"
    const unsafe_convert = Base.convert
else
    import Base.unsafe_convert
end

if VERSION < v"0.4.0-dev+3732"
    const calltypes = Dict{Symbol,Symbol}()
    for (k,v) in [(:Integer,:integer),
                   (:Signed,:signed),
                   (:Unsigned,:unsigned),
                   (:Int,:int),
                   (:Int8,:int8),
                   (:Int16,:int16),
                   (:Int32,:int32),
                   (:Int64,:int64),
                   (:Int128,:int128),
                   (:UInt,:uint),
                   (:UInt8,:uint8),
                   (:UInt16,:uint16),
                   (:UInt32,:uint32),
                   (:UInt64,:uint64),
                   (:UInt128,:uint128),
                   (:Float16,:float16),
                   (:Float32,:float32),
                   (:Float64,:float64),
                   (:Complex32,:complex32),
                   (:Complex64,:complex64),
                   (:Complex128,:complex128),
                   (:Char,:char),
                   (:Bool,:bool),
                   (:Symbol,:symbol)]
        calltypes[k] = v
    end
elseif VERSION < v"0.5.0-dev+3831"
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
        f = ex.args[1]
        if VERSION < v"0.4.0-dev+980" && (f === :Dict || (isexpr(f, :curly) && length(f.args) == 3 && f.args[1] === :Dict))
            ex = rewrite_dict(ex)
        elseif VERSION < v"0.4.0-dev+980" && (f === :OrderedDict || (isexpr(f, :curly) && length(f.args) == 3 && f.args[1] === :OrderedDict))
            ex = rewrite_ordereddict(ex)
        elseif VERSION < v"0.4.0-dev+129" && (f === :split || f === :rsplit) && length(ex.args) >= 4 && isexpr(ex.args[4], :kw)
            ex = rewrite_split(ex, f)
        elseif VERSION < v"0.4.0-dev+3732" && haskey(calltypes, f) && length(ex.args) > 1
            T = ex.args[1]
            if T in (:Complex32, :Complex64, :Complex128)
                ex = Expr(:call, calltypes[T], ex.args[2:end]...)
            elseif T == :Symbol
                ex = Expr(:call, calltypes[T], Expr(:call, :string, ex.args[2:end]...))
            else
                ex = Expr(:(::), Expr(:call, :convert, T, ex.args[2:end]...), T)
            end
        elseif VERSION < v"0.4.0-dev+3732" && (f === :map && haskey(calltypes, ex.args[2]) && length(ex.args) > 2)
            ex = Expr(:call, calltypes[ex.args[2]], ex.args[3:end]...)
        elseif VERSION < v"0.4.0-dev+1419" && isexpr(f, :curly) && f.args[1] === :Ptr && length(ex.args) == 2 && ex.args[2] == 0
            ex = Expr(:call, :zero, f)
        elseif VERSION < v"0.4.0-dev+4356" && f === :chol
            s = ex.args[3]
            if isexpr(s, :curly) && s.args[1] === :Val
                ex = Expr(:call, :chol, ex.args[2], s.args[2])
            end
        elseif VERSION < v"0.4.0-dev+4739" && isexpr(f, :curly) && (f.args[1] === :Vector || f.args[1] === :Array)
            if f.args[1] === :Vector && length(ex.args) == 1
                ex = Expr(:call, :Array, f.args[2], 0)
            else
                ex = Expr(:call, :Array, f.args[2], ex.args[2:end]...)
            end
        elseif VERSION < v"0.4.0-dev+4389" && f === :withenv
            rewrite_pairs_to_tuples!(ex)
        elseif VERSION < v"0.4.0-dev+1246" && f == :String
            ex = Expr(:call, :bytestring, ex.args[2:end]...)
        elseif VERSION < v"0.5.0-dev+4340" && length(ex.args) > 3 && istopsymbol(ex.args[1], :Base, :show)
            ex = rewrite_show(ex)
        end
        if VERSION < v"0.5.0-dev+4305"
            rewrite_iocontext!(ex)
        end
    elseif ex.head === :curly
        f = ex.args[1]
        if VERSION < v"0.4.0-dev+4319" && f === :Tuple
            args = ex.args[2:end]
            has_ellipsis = any(args) do arg
                isa(arg, Expr) && (arg.head === :...)
            end
            ex = if has_ellipsis
                Expr(:call, TopNode(:tuple), args...)
            else
                Expr(:tuple, args...)
            end
        elseif VERSION < v"0.4.0-dev+5379" && f === :Union
            ex = Expr(:call,:Union,ex.args[2:end]...)
        elseif ex == :(Ptr{Void})
            # Do no change Ptr{Void} to Ptr{Nothing}: 0.4.0-dev+768
            return ex
        end
    elseif ex.head === :macrocall
        f = ex.args[1]
        if VERSION < v"0.4.0-dev+4387" && f === symbol("@generated")
            f = ex.args[2]
            if isexpr(f, :function)
                ex = Expr(:stagedfunction, f.args...)
            end
        end
        if VERSION < v"0.5.0-dev+2129" && f === symbol("@boundscheck") && length(ex.args) == 2
            # Handle 0.5 single argument @boundscheck syntax. (0.4 has a two
            # arguments and a very diffferent meaning).  `nothing` is included
            # so we have a consistent return type.
            ex = :($(ex.args[2]); nothing)
        end
    elseif VERSION < v"0.4.0-dev+5322" && ex.head === :(::) && isa(ex.args[end], Symbol)
        # Replace Base.Timer with Compat.Timer2 in type declarations
        if istopsymbol(ex.args[end], :Base, :Timer)
            ex.args[end] = :(Compat.Timer2)
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
    if VERSION < v"0.4.0-dev+768" && ex === :Void
        return :Nothing
    end
    if VERSION < v"0.5.0-dev+1343" && (ex == :Future || ex == :RemoteChannel)
        return :RemoteRef
    end
    return ex
end
_compat(ex) = ex

macro compat(ex)
    esc(_compat(ex))
end

export @compat, @inline, @noinline

if VERSION < v"0.4.0-dev+656"
    include("nullable.jl")
end

if VERSION < v"0.4.0-dev+3864"
    function tryparse(T::Type{Float32}, s)
        r = Array(T,1)
        float32_isvalid(s, r) ? Nullable(r[1]) : Nullable{Float32}()
    end

    function tryparse(T::Type{Float64}, s)
        r = Array(T,1)
        float64_isvalid(s, r) ? Nullable(r[1]) : Nullable{Float64}()
    end

    function tryparse{T<:Integer}(::Type{T}, s, base=10)
        (2 <= base <= 62) || throw(ArgumentError("invalid base: base must be 2 ≤ base ≤ 62, got $base"))
        try
            Nullable(parse(T, s, base))
        catch
            Nullable{T}()
        end
    end
    export tryparse
end

if VERSION < v"0.4.0-dev+3844"
    @eval module Libc
        const FILE = Base.CFILE
        const calloc = Base.c_calloc
        const free = Base.c_free
        const malloc = Base.c_malloc
        const realloc = Base.c_realloc
        using Base: munmap, systemsleep
        export FILE, TmStruct, calloc, errno, flush_cstdio, free,
               gethostname, getpid, malloc, msync, realloc, strerror,
               strftime, strptime, systemsleep, time
    end
    @eval module Libdl
        using Base.Sys: dlext, dllist, dlpath
        export DL_LOAD_PATH, RTLD_DEEPBIND, RTLD_FIRST, RTLD_GLOBAL, RTLD_LAZY,
               RTLD_LOCAL, RTLD_NODELETE, RTLD_NOLOAD, RTLD_NOW, dlclose, dlext,
               dllist, dlopen, dlopen_e, dlpath, dlsym, dlsym_e, find_library
    end
    export Libc, Libdl
else
    import Base: Libc, Libdl
end

if VERSION < v"0.4.0-dev+2418"
    function findprev(A, start)
        for i = start:-1:1
            A[i] != 0 && return i
        end
        0
    end
    findlast(A) = findprev(A, length(A))
    function findprev(A, v, start)
        for i = start:-1:1
            A[i] == v && return i
        end
        0
    end
    findlast(A, v) = findprev(A, v, length(A))
    function findprev(testf::Function, A, start)
        for i = start:-1:1
            testf(A[i]) && return i
        end
        0
    end
    findlast(testf::Function, A) = findprev(testf, A, length(A))
    export findprev, findlast
end

if VERSION < v"0.4.0-dev+4524"
    isdiag(A::AbstractMatrix) = istril(A) && istriu(A)
    isdiag(x::Number) = true
    export isdiag
end

if VERSION < v"0.4.0-dev+4502"
    keytype{K,V}(::Associative{K,V}) = K
    valtype{K,V}(::Associative{K,V}) = V
    export keytype, valtype
end

if VERSION < v"0.4.0-dev+2254"
    immutable Val{T} end
    export Val
end

if VERSION < v"0.4.0-dev+2840"
    Base.qr(A, ::Type{Val{true}}; thin::Bool=true) =
        Base.qr(A, pivot=true, thin=thin)
    Base.qr(A, ::Type{Val{false}}; thin::Bool=true) =
        Base.qr(A, pivot=false, thin=thin)
    Base.qrfact(A, ::Type{Val{true}}) = Base.qrfact(A, pivot=true)
    Base.qrfact(A, ::Type{Val{false}}) = Base.qrfact(A, pivot=false)
    Base.qrfact!(A, ::Type{Val{true}}) = Base.qrfact!(A, pivot=true)
    Base.qrfact!(A, ::Type{Val{false}}) = Base.qrfact!(A, pivot=false)
end

if VERSION < v"0.4.0-dev+2823"
    fma(x::Number,y::Number,z::Number) = x*y + z
    export fma
end

if VERSION < v"0.4.0-dev+2861"
    muladd(x::Number,y::Number,z::Number) = x*y + z
    export muladd
end

if VERSION < v"0.4.0-dev+4734"
    function is_valid_utf32(str::Union(Vector{Char}, Vector{UInt32}))
        for i=1:length(str)
            @inbounds if !is_valid_char(reinterpret(UInt32, str[i])) ; return false ; end
        end
        return true
    end
    is_valid_utf32(str::UTF32String) = is_valid_utf32(str.data)
    export is_valid_utf32
end

if VERSION < v"0.4.0-dev+4939"
    import Base.isvalid
    isvalid(ch::Char)         = is_valid_char(ch)
    isvalid(str::ASCIIString) = is_valid_ascii(str)
    isvalid(str::UTF8String)  = is_valid_utf8(str)
    isvalid(str::UTF16String) = is_valid_utf16(str)
    isvalid(str::UTF32String) = is_valid_utf32(str)
    isvalid(::Type{Char}, ch)         = is_valid_char(ch)
    isvalid(::Type{ASCIIString}, str) = is_valid_ascii(str)
    isvalid(::Type{UTF8String},  str) = is_valid_utf8(str)
    isvalid(::Type{UTF16String}, str) = is_valid_utf16(str)
    isvalid(::Type{UTF32String}, str) = is_valid_utf32(str)
    export isvalid
end

if VERSION < v"0.4.0-dev+5322"
    include("timer.jl")
end

if VERSION < v"0.4.0-dev+5688"
    typealias Irrational MathConst
    @eval const $(symbol("@irrational")) = getfield(Base, symbol("@math_const"))
    export Irrational
else
    import Base.@irrational
end

if VERSION < v"0.4.0-dev+5305"
    function Base.gc_enable(on::Bool)
        state = ccall(:jl_gc_is_enabled, Cint, ()) != 0
        on ? gc_enable() : gc_disable()
        state
    end
end

if VERSION < v"0.4.0-dev+5697"
    @eval module Mmap
        function mmap{T,N}(io::IO,
            ::Type{Array{T,N}}=Vector{UInt8},
            dims::NTuple{N,Integer}=(div(filesize(io)-position(io),sizeof(T)),),
            offset::Integer=position(io); grow::Bool=true, shared::Bool=true)
            mmap_array(T,dims,io::IO,convert(FileOffset,offset))
        end

        mmap{T<:Array,N}(file::String,
                         ::Type{T}=Vector{UInt8},
                         dims::NTuple{N,Integer}=(div(filesize(file),sizeof(eltype(T))),),
                         offset::Integer=0; grow::Bool=true, shared::Bool=true) =
            open(io->mmap(io, T, dims, offset; grow=grow, shared=shared), file, isfile(file) ? "r+" : "w+")::Array{eltype(T),N}

        # using a length argument instead of dims
        mmap{T<:Array}(io::IO, ::Type{T}, len::Integer, offset::Integer=position(io); grow::Bool=true, shared::Bool=true) =
            mmap(io, T, (len,), offset; grow=grow, shared=shared)
        mmap{T<:Array}(file::String, ::Type{T}, len::Integer, offset::Integer=0; grow::Bool=true, shared::Bool=true) =
            open(io->mmap(io, T, (len,), offset; grow=grow, shared=shared), file, isfile(file) ? "r+" : "w+")::Vector{eltype(T)}

        # constructors for non-file-backed (anonymous) mmaps
        mmap{T<:Array,N}(::Type{T}, dims::NTuple{N,Integer}; shared::Bool=true) = mmap(Anonymous(), T, dims, 0; shared=shared)
        mmap{T<:Array}(::Type{T}, i::Integer...; shared::Bool=true) = mmap(Anonymous(), T, convert(Tuple{Vararg{Int}},i), 0; shared=shared)
    end
    export Mmap
end

if VERSION < v"0.4.0-dev+6521"
    __precompile__(isprecompilable::Bool = true) = nothing
    export __precompile__
end

if VERSION < v"0.4.0-dev+6506"
    include_dependency(::AbstractString) = nothing
    export include_dependency
end

if VERSION < v"0.4.0-dev+6425"
    typealias AbstractFloat FloatingPoint
    export AbstractFloat
end

if VERSION < v"0.4.0-dev+6068"
    Base.real{T<:Real}(::Type{T}) = T
    Base.real{T<:Real}(::Type{Complex{T}}) = T
end

if VERSION < v"0.4.0-dev+6578"
    rtoldefault{T<:AbstractFloat}(::Type{T}) = sqrt(eps(T))
    rtoldefault{T<:Real}(::Type{T}) = 0
    rtoldefault{T<:Number,S<:Number}(x::Union(T,Type{T}), y::Union(S,Type{S})) = rtoldefault(promote_type(real(T),real(S)))
    function Base.isapprox{T<:Number,S<:Number}(x::AbstractArray{T}, y::AbstractArray{S}; rtol::Real=rtoldefault(T,S), atol::Real=0, norm::Function=vecnorm)
        d = norm(x - y)
        return isfinite(d) ? d <= atol + rtol*max(norm(x), norm(y)) : x == y
    end
    const ≈ = isapprox
    ≉(x,y) = !(x ≈ y)
    export ≈, ≉
end

# Use as Compat.Linspace to get improved linspace in both 0.3 and 0.4
if VERSION < v"0.4.0-dev+6110"
    include("linspace.jl")
    const linspace = LazyLinSpace.linspace
else
    const linspace = Base.linspace
end

if VERSION < v"0.4.0-dev+436"
    export UDPSocket
    const UDPSocket = UdpSocket
    export TCPSocket
    const TCPSocket = Base.TcpSocket
end

if VERSION < v"0.4.0-dev+2340"
    export Base64EncodePipe
    const Base64EncodePipe = Base64Pipe
end

if VERSION < v"0.4.0-dev+3837"
    export OutOfMemoryError
    const OutOfMemoryError = MemoryError
end


if VERSION < v"0.4.0-dev+4389"
    export withenv
    # temporarily set and then restore an environment value
    function withenv(f::Function, keyvals...)
        old = Dict{typeof(first(first(keyvals))),Any}()
        for (key,val) in keyvals
            old[key] = get(ENV, key, nothing)
            val !== nothing ? (ENV[key]=val) : Base._unsetenv(key)
        end
        try f()
        finally
            for (key,val) in old
                val !== nothing ? (ENV[key]=val) : Base._unsetenv(key)
            end
        end
    end
    withenv(f::Function) = f() # handle empty keyvals case; see #10853
end

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

if VERSION < v"0.4.0-dev+1653"
    function rm_recursive(path)
        if islink(path) || !isdir(path)
            rm(path)
        else
            for p in readdir(path)
                rm_recursive(joinpath(path, p))
            end
            rm(path)
        end
    end

    function Base.mktemp(fn::Function)
        (tmp_path, tmp_io) = mktemp()
        try
            fn(tmp_path, tmp_io)
        finally
            close(tmp_io)
            rm(tmp_path)
        end
    end

    function Base.mktempdir(fn::Function)
        tmpdir = mktempdir()
        try
            fn(tmpdir)
        finally
            rm_recursive(tmpdir)
        end
    end
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
    const AsyncCondition = Base.SingleAsyncWork
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
if VERSION < v"0.4.0-dev+1373" # PR #8854
    function make_seed()
        try
            @static if is_unix()
                seed = open("/dev/urandom") do io
                    a = Array(UInt32, 4)
                    read!(io, a)
                    a
                end
            elseif is_windows()
                seed = let a = zeros(UInt32, 2)
                    Base.dSFMT.win32_SystemFunction036!(a)
                    a
                end
            end
            return seed
        catch
            println(STDERR, "Entropy pool not available to seed RNG; using ad-hoc entropy sources.")
            seed = reinterpret(UInt64, time())
            seed = hash(seed, convert(UInt64, getpid()))
            try
                seed = hash(seed, parse(UInt64, readall(`ifconfig` |> `sha1sum`)[1:40], 16))
            end
            return seed
        end
    end
    if VERSION < v"0.4.0-dev+1352" # PR #8832
        function srand(r::MersenneTwister, seed::Vector{UInt32})
            r.seed = seed
            Base.dSFMT.dsfmt_init_by_array(r.state, seed)
            return r
        end
    else
        function srand(r::MersenneTwister, seed::Vector{UInt32})
            r.seed = seed
            Base.dSFMT.dsfmt_init_by_array(r.state, seed)
            r.idx = Base.dSFMT.dsfmt_get_min_array_size()
            return r
        end
        rand(r::MersenneTwister, ::Type{UInt32})  = reinterpret(UInt64, Base.Random.rand_close1_open2(r)) % UInt32
    end
    srand(r::MersenneTwister, seed::Union(UInt32,Int32)) = srand(r, UInt32[seed])
    srand(r::MersenneTwister, seed::Union(UInt64,Int64)) =
        srand(r, UInt32[seed & 0xffffffff, seed >> 32])
    srand(r::MersenneTwister) = srand(r, make_seed())

    rand(r::MersenneTwister, ::Type{UInt8})   = rand(r, UInt32) % UInt8
    rand(r::MersenneTwister, ::Type{UInt16})  = rand(r, UInt32) % UInt16
    rand(r::MersenneTwister, ::Type{UInt32})  = reinterpret(UInt64, rand(r)) % UInt32
    rand(r::MersenneTwister, ::Type{UInt64})  = convert(UInt64,rand(r, UInt32)) <<32 | rand(r, UInt32)
    rand(r::MersenneTwister, ::Type{UInt128}) = convert(UInt128,rand(r, UInt64))<<64 | rand(r, UInt64)

    for (T,Tu) in ((Int8,UInt8),(Int16,UInt16),(Int32,UInt32),(Int64,UInt64),(Int128,UInt128))
        @eval rand(r::MersenneTwister, ::Type{$T}) = reinterpret($T, rand(r, $Tu))
    end
    rand(r::MersenneTwister, ::Type{Float64}) = rand(r)
    rand(r::MersenneTwister, ::Type{Float64}) = rand(r)
    rand{T<:Union(Float16,Float32)}(r::MersenneTwister, ::Type{T}) = convert(T, rand(r))
    rand{T<:Real}(r::AbstractRNG, ::Type{Complex{T}}) = complex(rand(r, T), rand(r, T))
    if VERSION < v"0.4.0-dev+1296" # commit 89befafa7deded0119d0186ef116e03ec91b9680
        function rand!{T<:Number}(r::AbstractRNG, A::AbstractArray{T})
            for i=1:length(A)
                @inbounds A[i] = rand(r, T)
            end
            A
        end
    end
    rand(r::AbstractRNG, T::Type, dims::Dims) = rand!(r, Array(T, dims))
    rand(r::AbstractRNG, T::Type, d1::Int, dims::Int...) = rand!(r, Array(T, d1, dims...))
    rand(r::MersenneTwister, ::Type{Bool}) = ((rand(r, UInt32) & 1) == 1)
end

if VERSION < v"0.4.0-dev+551" # PR #8320
    srand() = srand(make_seed())
end

if VERSION < v"0.4.0-dev+1884"
    randexp(rng::MersenneTwister) = Base.Random.randmtzig_exprnd(rng)
    randexp() = Base.Random.randmtzig_exprnd()
    export randexp
end

if isdefined(Core, :String) && isdefined(Core, :AbstractString)
    # Not exported in order to not break code on 0.5
    typealias UTF8String Core.String
    typealias ASCIIString Core.String
else
    typealias String Base.ByteString
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

end # module
