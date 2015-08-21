# This file is a part of Julia. License is MIT: http://julialang.org/license

## symbols ##

symbol(s::Symbol) = s
symbol(s::UTF8String) = symbol(s.data)
symbol(a::Array{UInt8,1}) =
    ccall(:jl_symbol_n, Any, (Ptr{UInt8}, Int32), a, length(a))::Symbol
symbol(x...) = symbol(string(x...))

gensym() = ccall(:jl_gensym, Any, ())::Symbol

gensym(s::UTF8String) = gensym(s.data)
gensym(a::Array{UInt8,1}) =
    ccall(:jl_tagged_gensym, Any, (Ptr{UInt8}, Int32), a, length(a))::Symbol
gensym(ss::UTF8String...) = map(gensym, ss)
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
astcopy(x::Union{SymbolNode,Expr}) = copy(x)
astcopy(x::Array{Any,1}) = Any[astcopy(a) for a in x]
astcopy(x) = x

==(x::Expr, y::Expr) = x.head === y.head && x.args == y.args
==(x::QuoteNode, y::QuoteNode) = x.value == y.value
==(x::SymbolNode, y::SymbolNode) = x.name === y.name && x.typ === y.typ

expand(x) = ccall(:jl_expand, Any, (Any,), x)
macroexpand(x) = ccall(:jl_macroexpand, Any, (Any,), x)

## misc syntax ##

macro eval(x)
    :($(esc(:eval))($(Expr(:quote,x))))
end

macro inline(ex)
    esc(isa(ex, Expr) ? pushmeta!(ex, :inline) : ex)
end

macro noinline(ex)
    esc(isa(ex, Expr) ? pushmeta!(ex, :noinline) : ex)
end

macro pure(ex)
    esc(isa(ex, Expr) ? pushmeta!(ex, :pure) : ex)
end

"""
    @propagate_inbounds(ex)

Tells the compiler to inline a function while retaining the caller's inbounds context.
"""
macro propagate_inbounds(ex)
    if isa(ex, Expr)
        pushmeta!(ex, :inline)
        pushmeta!(ex, :propagate_inbounds)
        esc(ex)
    else
        esc(ex)
    end
end

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
    Expr(:localize, expr, v...)
end

function pushmeta!(ex::Expr, sym::Symbol, args::Any...)
    if isempty(args)
        tag = sym
    else
        tag = Expr(sym, args...)
    end
    found, metaex = findmeta(ex)
    if found
        push!(metaex.args, tag)
    else
        body::Expr = ex.args[2]
        unshift!(body.args, Expr(:meta, tag))
    end
    ex
end

function popmeta!(body::Expr, sym::Symbol)
    body.head == :block || return false, []
    found, metaex = findmeta_block(body)
    if !found
        return false, []
    end
    metaargs = metaex.args
    for i = 1:length(metaargs)
        if isa(metaargs[i], Symbol) && (metaargs[i]::Symbol) == sym
            deleteat!(metaargs, i)
            return true, []
        elseif isa(metaargs[i], Expr) && (metaargs[i]::Expr).head == sym
            ret = (metaargs[i]::Expr).args
            deleteat!(metaargs, i)
            return true, ret
        end
    end
    false, []
end
popmeta!(arg, sym) = (false, [])

function findmeta(ex::Expr)
    if ex.head == :function || (ex.head == :(=) && typeof(ex.args[1]) == Expr && ex.args[1].head == :call)
        body::Expr = ex.args[2]
        body.head == :block || error(body, " is not a block expression")
        return findmeta_block(ex)
    end
    error(ex, " is not a function expression")
end

function findmeta_block(ex::Expr)
    for a in ex.args
        if isa(a, Expr)
            if (a::Expr).head == :meta
                return true, a::Expr
            elseif (a::Expr).head == :block
                found, exb = findmeta_block(a)
                if found
                    return found, exb
                end
            end
        end
    end
    return false, Expr(:block)
end

remove_linenums!(ex) = ex
function remove_linenums!(ex::Expr)
    filter!(x->!((isa(x,Expr) && is(x.head,:line)) || isa(x,LineNumberNode)), ex.args)
    for subex in ex.args
        remove_linenums!(subex)
    end
    ex
end
