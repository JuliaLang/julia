# This file is a part of Julia. License is MIT: http://julialang.org/license

abstract IO

typealias Callable Union(Function,DataType)

const Bottom = Union()

# constructors for Core types in boot.jl
call(T::Type{BoundsError}) = Core.call(T)
call(T::Type{BoundsError}, args...) = Core.call(T, args...)
call(T::Type{DivideError}) = Core.call(T)
call(T::Type{DomainError}) = Core.call(T)
call(T::Type{OverflowError}) = Core.call(T)
call(T::Type{InexactError}) = Core.call(T)
call(T::Type{OutOfMemoryError}) = Core.call(T)
call(T::Type{StackOverflowError}) = Core.call(T)
call(T::Type{UndefRefError}) = Core.call(T)
call(T::Type{UndefVarError}, var::Symbol) = Core.call(T, var)
call(T::Type{InterruptException}) = Core.call(T)
call(T::Type{SymbolNode}, name::Symbol, t::ANY) = Core.call(T, name, t)
call(T::Type{GlobalRef}, modu, name::Symbol) = Core.call(T, modu, name)
call(T::Type{ASCIIString}, d::Array{UInt8,1}) = Core.call(T, d)
call(T::Type{UTF8String}, d::Array{UInt8,1}) = Core.call(T, d)
call(T::Type{TypeVar}, args...) = Core.call(T, args...)
call(T::Type{TypeConstructor}, args...) = Core.call(T, args...)
call(T::Type{Expr}, args::ANY...) = _expr(args...)
call(T::Type{LineNumberNode}, n::Int) = Core.call(T, n)
call(T::Type{LabelNode}, n::Int) = Core.call(T, n)
call(T::Type{GotoNode}, n::Int) = Core.call(T, n)
call(T::Type{QuoteNode}, x::ANY) = Core.call(T, x)
call(T::Type{NewvarNode}, s::Symbol) = Core.call(T, s)
call(T::Type{TopNode}, s::Symbol) = Core.call(T, s)
call(T::Type{Module}, args...) = Core.call(T, args...)
call(T::Type{Task}, f::ANY) = Core.call(T, f)
call(T::Type{GenSym}, n::Int) = Core.call(T, n)
call(T::Type{WeakRef}) = Core.call(T)
call(T::Type{WeakRef}, v::ANY) = Core.call(T, v)

# The specialization for 1 arg is important
# when running with --inline=no, see #11158
call{T}(::Type{T}, arg) = convert(T, arg)::T
call{T}(::Type{T}, args...) = convert(T, args...)::T

convert{T}(::Type{T}, x::T) = x

convert(::Type{Tuple{}}, ::Tuple{}) = ()
convert(::Type{Tuple}, x::Tuple) = x
convert{T}(::Type{Tuple{Vararg{T}}}, x::Tuple) = cnvt_all(T, x...)
cnvt_all(T) = ()
cnvt_all(T, x, rest...) = tuple(convert(T,x), cnvt_all(T, rest...)...)

macro generated(f)
    isa(f, Expr) || error("invalid syntax; @generated must be used with a function definition")
    if is(f.head, :function) || isdefined(:length) && is(f.head, :(=)) && length(f.args) == 2 && f.args[1].head == :call
        f.head = :stagedfunction
        return Expr(:escape, f)
    else
        error("invalid syntax; @generated must be used with a function definition")
    end
end


@generated function tuple_type_head{T<:Tuple}(::Type{T})
    T.parameters[1]
end

isvarargtype(t::ANY) = isa(t,DataType)&&is((t::DataType).name,Vararg.name)
isvatuple(t::DataType) = (n = length(t.parameters); n > 0 && isvarargtype(t.parameters[n]))
unwrapva(t::ANY) = isvarargtype(t) ? t.parameters[1] : t

@generated function tuple_type_tail{T<:Tuple}(::Type{T})
    if isvatuple(T) && length(T.parameters) == 1
        return T
    end
    Tuple{argtail(T.parameters...)...}
end

argtail(x, rest...) = rest
tail(x::Tuple) = argtail(x...)

convert{T<:Tuple{Any,Vararg{Any}}}(::Type{T}, x::Tuple{Any, Vararg{Any}}) =
    tuple(convert(tuple_type_head(T),x[1]), convert(tuple_type_tail(T), tail(x))...)

oftype(x,c) = convert(typeof(x),c)

unsigned(x::Int) = reinterpret(UInt, x)
signed(x::UInt) = reinterpret(Int, x)

# conversions used by ccall
ptr_arg_cconvert{T}(::Type{Ptr{T}}, x) = cconvert(T, x)
ptr_arg_unsafe_convert{T}(::Type{Ptr{T}}, x) = unsafe_convert(T, x)
ptr_arg_unsafe_convert(::Type{Ptr{Void}}, x) = x

cconvert(T::Type, x) = convert(T, x) # do the conversion eagerly in most cases
cconvert{P<:Ptr}(::Type{P}, x) = x # but defer the conversion to Ptr to unsafe_convert
unsafe_convert{T}(::Type{T}, x::T) = x # unsafe_convert (like convert) defaults to assuming the convert occurred
unsafe_convert{P<:Ptr}(::Type{P}, x::Ptr) = convert(P, x)

reinterpret{T,S}(::Type{T}, x::S) = box(T,unbox(S,x))

sizeof(x) = Core.sizeof(x)

function append_any(xs...)
    # used by apply() and quote
    # must be a separate function from append(), since apply() needs this
    # exact function.
    out = Array(Any, 4)
    l = 4
    i = 1
    for x in xs
        for y in x
            if i > l
                ccall(:jl_array_grow_end, Void, (Any, UInt), out, 16)
                l += 16
            end
            arrayset(out, y, i)
            i += 1
        end
    end
    ccall(:jl_array_del_end, Void, (Any, UInt), out, l-i+1)
    out
end

# simple Array{Any} operations needed for bootstrap
setindex!(A::Array{Any}, x::ANY, i::Real) = arrayset(A, x, to_index(i))

function length_checked_equal(args...)
    n = length(args[1])
    for i=2:length(args)
        if length(args[i]) != n
            error("argument dimensions must match")
        end
    end
    n
end

function map(f::Function, a::Array{Any,1})
    A = Array(Any,length(a))
    for i=1:length(a)
        A[i] = f(a[i])
    end
    A
end

function precompile(f::ANY, args::Tuple)
    if isa(f,DataType)
        args = tuple(Type{f}, args...)
        f = f.name.module.call
    end
    if isgeneric(f)
        ccall(:jl_compile_hint, Void, (Any, Any), f, Tuple{args...})
    end
end

esc(e::ANY) = Expr(:escape, e)

macro boundscheck(yesno,blk)
    # hack: use this syntax since it avoids introducing line numbers
    :($(Expr(:boundscheck,yesno));
      $(esc(blk));
      $(Expr(:boundscheck,:pop)))
end

macro inbounds(blk)
    :(@boundscheck false $(esc(blk)))
end

macro label(name::Symbol)
    Expr(:symboliclabel, name)
end

macro goto(name::Symbol)
    Expr(:symbolicgoto, name)
end

call{T,N}(::Type{Array{T}}, d::NTuple{N,Int}) =
    ccall(:jl_new_array, Array{T,N}, (Any,Any), Array{T,N}, d)
call{T}(::Type{Array{T}}, d::Integer...) = Array{T}(convert(Tuple{Vararg{Int}}, d))

call{T}(::Type{Array{T}}, m::Integer) =
    ccall(:jl_alloc_array_1d, Array{T,1}, (Any,Int), Array{T,1}, m)
call{T}(::Type{Array{T}}, m::Integer, n::Integer) =
    ccall(:jl_alloc_array_2d, Array{T,2}, (Any,Int,Int), Array{T,2}, m, n)
call{T}(::Type{Array{T}}, m::Integer, n::Integer, o::Integer) =
    ccall(:jl_alloc_array_3d, Array{T,3}, (Any,Int,Int,Int), Array{T,3}, m, n, o)

# TODO: possibly turn these into deprecations
Array{T,N}(::Type{T}, d::NTuple{N,Int}) = Array{T}(d)
Array{T}(::Type{T}, d::Integer...)      = Array{T}(convert(Tuple{Vararg{Int}}, d))
Array{T}(::Type{T}, m::Integer)                       = Array{T}(m)
Array{T}(::Type{T}, m::Integer,n::Integer)            = Array{T}(m,n)
Array{T}(::Type{T}, m::Integer,n::Integer,o::Integer) = Array{T}(m,n,o)

# SimpleVector

function getindex(v::SimpleVector, i::Int)
    if !(1 <= i <= length(v))
        throw(BoundsError(v,i))
    end
    x = unsafe_load(convert(Ptr{Ptr{Void}},data_pointer_from_objref(v)) + i*sizeof(Ptr))
    x == C_NULL && throw(UndefRefError())
    return unsafe_pointer_to_objref(x)
end

length(v::SimpleVector) = v.length
endof(v::SimpleVector) = v.length
start(v::SimpleVector) = 1
next(v::SimpleVector,i) = (v[i],i+1)
done(v::SimpleVector,i) = (i > v.length)
isempty(v::SimpleVector) = (v.length == 0)

function ==(v1::SimpleVector, v2::SimpleVector)
    length(v1)==length(v2) || return false
    for i = 1:length(v1)
        v1[i] == v2[i] || return false
    end
    return true
end

function map(f, v::SimpleVector)
    A = Array(Any,length(v))
    for i=1:length(v)
        A[i] = f(v[i])
    end
    A
end

getindex(v::SimpleVector, I::AbstractArray) = svec(map(i->v[i],I)...)

function isassigned(v::SimpleVector, i::Int)
    1 <= i <= length(v) || return false
    x = unsafe_load(convert(Ptr{Ptr{Void}},data_pointer_from_objref(v)) + i*sizeof(Ptr))
    return x != C_NULL
end

# index colon
type Colon
end
const (:) = Colon()


start(A::Array) = 1
next(a::Array,i) = (a[i],i+1)
done(a::Array,i) = (i > length(a))
length(a::Array) = arraylen(a)
gensym() = ccall(:jl_gensym, Any, ())::Symbol
getindex(A::Array, i0::Int) = arrayref(A,i0)
function vect{T}(args::T...)
    A = Array(T, length(args))
    for i=1:length(A)
        A[i] = args[i]
    end
    A
end

# this should be only tuples/vars
function gen_var(f, ex, acc)
    if isa(ex, Expr)
        for a in ex.args
            acc = gen_var(f, a,acc)
        end
        acc
    elseif isa(ex,Symbol)
        :($acc; $(f(ex)))
    else
        error("unknown iteration spec ?")
    end
end

function gen_comp(T, body, iter0, isdict::Bool)
    niter = length(iter0)
    sz = Array(Any,niter) # dim tuple
    sz0 = Array(Any,niter) # dim tuple if an input is empty
    iterblock = Array(Any,niter) # canonicalized iteration spec
    preheader = :() # assignment for iterables to avoid repeated evaluation
    preheader_colon = :()
    isempty = :(false)
    itst = Array(Any,niter) # names for iterator states
    ncolon = 0

    for i=1:niter # precompute this, we don't have push!
        ncolon += (iter0[i] === :(:))
    end

    colons = Array(Any,ncolon)
    iscolon = Array(Any,niter)
    needs_firsteval = T === nothing # do we need to unroll the 1st iteration
    needs_fallback = T === nothing # do we need to check types
    icol = 1
    for i = 1:niter
        it = iter0[i]
        iscol = false
        if it === :(:)
            colonname = gensym()
            colons[icol] = colonname
            it = :($colonname = 1:size(v0,$icol))
            icol += 1
            iscol = true
            needs_firsteval = true
        end
        iscolon[i] = iscol
        # generate anonymous name for unused iterators
        if !isa(it,Expr) || it.head !== :(=)
            it = :($(gensym()) = $it)
        end

        name = gensym()
        itst[i] = gensym()
        sz[i] = :(length($name))
        itname = it.args[2]
        preheader = gen_var(ex -> :(local $(esc(ex))), it.args[1], preheader)
        if iscol
            preheader_colon = quote
                $preheader_colon
                $name = $itname
                $(itst[i]) = start($name)
                $(it.args[1]), $(itst[i]) = next($name, $(itst[i]))
            end
            sz0[i] = :(0)
            iterblock[i] = :($(it.args[1]) = $name)
        else
            sz0[i] = :(length($name))
            preheader = :($preheader; $name = $(esc(itname)))
            isempty = :($isempty | (length($name) == 0))
            iterblock[i] = :($(esc(it.args[1])) = $name)
        end
    end

    if isdict
        key = body.args[1]
        body = body.args[2]
        if T !== nothing
            KT = T.args[1]
            T = T.args[2]
        end
    end

    #isempty = foldl((x,i) -> :($x | ($i == 0)), :(false), sz)

    loopexpr = if isdict
        quote
            index = $(esc(key))
            v = $(esc(body))
        end
    else
        if ncolon > 0
            :(v = $(Expr(:call, TopNode(:getindex), esc(body), colons...)))
        else
            :(v = $(esc(body)))
        end
    end
    if needs_firsteval
        first_it = :()
        for i = niter:-1:1
            iscolon[i] && continue
            it = iterblock[i]
            itname = it.args[2]
            first_it = quote
                $first_it
                $(itst[i]) = start($itname)
                $(it.args[1]), $(itst[i]) = next($itname, $(itst[i]))
            end
        end
        init = if isdict
            quote
                index = $(esc(key))
                v = $(esc(body))
                KT = typeof(index)
                T = typeof(v)
                result = Dict{KT,T}()
            end
        else
            colon_eval = if ncolon > 0
                quote
                    $preheader_colon
                    v = $(Expr(:call, TopNode(:getindex), esc(body), colons...))
                end
            else
                :(v = v0)
            end
            quote
                index = 1
                v0 = $(esc(body))
                $colon_eval
                $(if needs_fallback
                  quote
                  T = typeof(v)
                  result = Array(T, $(sz...))
                  end
                  else
                  :(result = Array($(esc(T)), $(sz...)))
                  end)
            end
        end
        fallback =
        if isdict
            quote
                S = typeof(v)
                KS = typeof(index)
                if !(S <: T && KS <: KT)
                    T = typejoin(S,T)
                    KT = typejoin(KS,KT)
                    result = convert(Dict{KT,T}, result)
                end
            end
        else
            quote
                S = typeof(v)
                if !(S <: T)
                    T = typejoin(S,T)
                    result_next = similar(result,T)
                    copy!(result_next, 1, result, 1, index-1)
                    result = result_next
                end
            end
        end

        header = quote
            $first_it
            $init
            @goto inner_loop
        end
        loopexpr = quote
            $loopexpr
            $(needs_fallback ? fallback : :())
            @label inner_loop
        end
    else
        header = if isdict
            quote
                result = Dict{$(esc(KT)),$(esc(T))}()
            end
        else
            quote
                index = 1
                result = Array($(esc(T)), $(sz...))
            end
        end
    end
    loopexpr = quote
        $loopexpr
        result[index] = v
    end
    if !isdict
        loopexpr = :($loopexpr; index += 1)
    end
    for i = 1:niter
        it = iterblock[i]
        itname = it.args[2]
        loopexpr = quote
            $(itst[i]) = start($itname)
            while !done($itname, $(itst[i]))
                tup = next($itname, $(itst[i]))
                $(itst[i]) = tup[2]
                $(gen_var(ex -> esc(NewvarNode(ex)), it.args[1], :())) # ensure new bindings if we capture the it variable
                $(it.args[1]) = tup[1]
                $loopexpr
            end
        end
    end
    q = if needs_firsteval
        quote
            let
                $preheader
                if $isempty
                    $(isdict ? :(Dict{Union(),Union()}()) : :(Array(Union(), $(sz0...))))
                else
                    $header
                    $loopexpr
                    result
                end
            end
        end
    else
        quote
            let
                $preheader
                $header
                $loopexpr
                result
            end
        end
    end
    q
end

macro comprehension(body, iter...)
    gen_comp(nothing,body,iter,false)
end

macro typed_comprehension(T, body, iter...)
    gen_comp(T,body,iter,false)
end

macro dict_comprehension(body,iter...)
    gen_comp(nothing,body,iter,true)
end

macro typed_dict_comprehension(T,body,iter...)
    gen_comp(T,body,iter,true)
end
