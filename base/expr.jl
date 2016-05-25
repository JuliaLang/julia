# This file is a part of Julia. License is MIT: http://julialang.org/license

## symbols ##

gensym() = ccall(:jl_gensym, Ref{Symbol}, ())

gensym(s::String) = gensym(s.data)
gensym(a::Array{UInt8,1}) =
    ccall(:jl_tagged_gensym, Ref{Symbol}, (Ptr{UInt8}, Int32), a, length(a))
gensym(ss::String...) = map(gensym, ss)
gensym(s::Symbol) =
    ccall(:jl_tagged_gensym, Ref{Symbol}, (Ptr{UInt8}, Int32), s, ccall(:strlen, Csize_t, (Ptr{UInt8},), s))

macro gensym(names...)
    blk = Expr(:block)
    for name in names
        push!(blk.args, :($(esc(name)) = gensym($(string(name)))))
    end
    push!(blk.args, :nothing)
    return blk
end

## expressions ##

copy(e::Expr) = (n = Expr(e.head);
                 n.args = copy_exprargs(e.args);
                 n.typ = e.typ;
                 n)

# copy parts of an AST that the compiler mutates
copy_exprs(x::Expr) = copy(x)
copy_exprs(x::ANY) = x
copy_exprargs(x::Array{Any,1}) = Any[copy_exprs(a) for a in x]

==(x::Expr, y::Expr) = x.head === y.head && isequal(x.args, y.args)
==(x::QuoteNode, y::QuoteNode) = isequal(x.value, y.value)

expand(x::ANY) = ccall(:jl_expand, Any, (Any,), x)
macroexpand(x::ANY) = ccall(:jl_macroexpand, Any, (Any,), x)

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

"""
Tells the compiler to apply the polyhedral optimizer Polly to a function.
"""
macro polly(ex)
    esc(isa(ex, Expr) ? pushmeta!(ex, :polly) : ex)
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
    elseif isa(e,Expr) && e.head !== :quote && e.head !== :top && e.head !== :core
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
    idx, exargs = findmeta(ex)
    if idx != 0
        push!(exargs[idx].args, tag)
    else
        body::Expr = ex.args[2]
        unshift!(body.args, Expr(:meta, tag))
    end
    ex
end

function popmeta!(body::Expr, sym::Symbol)
    body.head == :block || return false, []
    popmeta!(body.args, sym)
end
popmeta!(arg, sym) = (false, [])
function popmeta!(body::Array{Any,1}, sym::Symbol)
    idx, args = findmeta_block(body)
    if idx == 0
        return false, []
    end
    metaex = args[idx]
    metaargs = metaex.args
    for i = 1:length(metaargs)
        if isa(metaargs[i], Symbol) && (metaargs[i]::Symbol) == sym
            deleteat!(metaargs, i)
            isempty(metaargs) && deleteat!(args, idx)
            return true, []
        elseif isa(metaargs[i], Expr) && (metaargs[i]::Expr).head == sym
            ret = (metaargs[i]::Expr).args
            deleteat!(metaargs, i)
            isempty(metaargs) && deleteat!(args, idx)
            return true, ret
        end
    end
    false, []
end

function findmeta(ex::Expr)
    if ex.head == :function || (ex.head == :(=) && typeof(ex.args[1]) == Expr && ex.args[1].head == :call)
        body::Expr = ex.args[2]
        body.head == :block || error(body, " is not a block expression")
        return findmeta_block(ex.args)
    end
    error(ex, " is not a function expression")
end

findmeta(ex::Array{Any,1}) = findmeta_block(ex)

function findmeta_block(exargs)
    for i = 1:length(exargs)
        a = exargs[i]
        if isa(a, Expr)
            if (a::Expr).head == :meta
                return i, exargs
            elseif (a::Expr).head == :block
                idx, exa = findmeta_block(a.args)
                if idx != 0
                    return idx, exa
                end
            end
        end
    end
    return 0, []
end

remove_linenums!(ex) = ex
function remove_linenums!(ex::Expr)
    filter!(x->!((isa(x,Expr) && is(x.head,:line)) || isa(x,LineNumberNode)), ex.args)
    for subex in ex.args
        remove_linenums!(subex)
    end
    ex
end
