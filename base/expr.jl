# This file is a part of Julia. License is MIT: http://julialang.org/license

## symbols ##

symbol(s::Symbol) = s
symbol(s::ASCIIString) = symbol(s.data)
symbol(s::UTF8String) = symbol(s.data)
symbol(a::Array{UInt8,1}) =
    ccall(:jl_symbol_n, Any, (Ptr{UInt8}, Int32), a, length(a))::Symbol
symbol(x...) = symbol(string(x...))

gensym(s::ASCIIString) = gensym(s.data)
gensym(s::UTF8String) = gensym(s.data)
gensym(a::Array{UInt8,1}) =
    ccall(:jl_tagged_gensym, Any, (Ptr{UInt8}, Int32), a, length(a))::Symbol
gensym(ss::Union(ASCIIString, UTF8String)...) = map(gensym, ss)
gensym(s::Symbol) =
    ccall(:jl_tagged_gensym, Any, (Ptr{UInt8}, Int32), s, ccall(:strlen, Csize_t, (Ptr{UInt8},), s))::Symbol

macro gensym(names...)
    blk = Expr(:block)
    for name in names
        push!(blk.args, :($(esc(name)) = gensym($(string(name)))))
    end
    push!(blk.args, :nothing)
    return blk
end

## expressions ##

splicedexpr(hd::Symbol, args::Array{Any,1}) = (e=Expr(hd); e.args=args; e)
copy(e::Expr) = (n = Expr(e.head);
                 n.args = astcopy(e.args);
                 n.typ = e.typ;
                 n)
copy(s::SymbolNode) = SymbolNode(s.name, s.typ)

# copy parts of an AST that the compiler mutates
astcopy(x::Union(SymbolNode,Expr)) = copy(x)
astcopy(x::Array{Any,1}) = map(a->astcopy(a),x)
astcopy(x) = x

==(x::Expr, y::Expr) = x.head === y.head && x.args == y.args
==(x::QuoteNode, y::QuoteNode) = x.value == y.value

function show(io::IO, tv::TypeVar)
    if !is(tv.lb, Bottom)
        show(io, tv.lb)
        print(io, "<:")
    end
    write(io, tv.name)
    if !is(tv.ub, Any)
        print(io, "<:")
        show(io, tv.ub)
    end
end

expand(x) = ccall(:jl_expand, Any, (Any,), x)
macroexpand(x) = ccall(:jl_macroexpand, Any, (Any,), x)

## misc syntax ##

macro eval(x)
    :($(esc(:eval))($(Expr(:quote,x))))
end

macro inline(ex)
    esc(_inline(ex))
end

_inline(ex::Expr) = pushmeta!(ex, :inline)
_inline(arg) = arg

macro noinline(ex)
    esc(_noinline(ex))
end

_noinline(ex::Expr) = pushmeta!(ex, :noinline)
_noinline(arg) = arg

## some macro utilities ##

find_vars(e) = find_vars(e, [])
function find_vars(e, lst)
    if isa(e,Symbol)
        if current_module()===Main && isdefined(e)
            # Main runs on process 1, so send globals from there, excluding
            # things defined in Base.
            if !isdefined(Base,e) || eval(Base,e)!==eval(current_module(),e)
                push!(lst, e)
            end
        end
    elseif isa(e,Expr) && e.head !== :quote && e.head !== :top
        for x in e.args
            find_vars(x,lst)
        end
    end
    lst
end

# wrap an expression in "let a=a,b=b,..." for each var it references
localize_vars(expr) = localize_vars(expr, true)
function localize_vars(expr, esca)
    v = find_vars(expr)
    # requires a special feature of the front end that knows how to insert
    # the correct variables. the list of free variables cannot be computed
    # from a macro.
    if esca
        v = map(esc,v)
    end
    Expr(:localize, :(()->($expr)), v...)
end

function pushmeta!(ex::Expr, sym::Symbol, args::Any...)
    if length(args) == 0
        tag = sym
    else
        tag = Expr(sym, args...)
    end

    if ex.head == :function
        body::Expr = ex.args[2]
        if !isempty(body.args) && isa(body.args[1], Expr) && (body.args[1]::Expr).head == :meta
            push!((body.args[1]::Expr).args, tag)
        elseif isempty(body.args)
            push!(body.args, Expr(:meta, tag))
            push!(body.args, nothing)
        else
            unshift!(body.args, Expr(:meta, tag))
        end
    elseif (ex.head == :(=) && typeof(ex.args[1]) == Expr && ex.args[1].head == :call)
        ex = Expr(:function, ex.args[1], Expr(:block, Expr(:meta, tag), ex.args[2]))
#     else
#         ex = Expr(:withmeta, ex, sym)
    end
    ex
end

function popmeta!(body::Expr, sym::Symbol)
    if isa(body.args[1],Expr) && (body.args[1]::Expr).head === :meta
        metaargs = (body.args[1]::Expr).args
        for i = 1:length(metaargs)
            if (isa(metaargs[i], Symbol) && metaargs[i] == sym) ||
               (isa(metaargs[i], Expr) && metaargs[i].head == sym)
                if length(metaargs) == 1
                    shift!(body.args)        # get rid of :meta Expr
                else
                    deleteat!(metaargs, i)   # delete this portion of the metadata
                end

                if isa(metaargs[i], Symbol)
                    return (true, [])
                elseif isa(metaargs[i], Expr)
                    return (true, metaargs[i].args)
                end
            end
        end
    end
    return (false, [])
end
popmeta!(arg, sym) = (false, [])
