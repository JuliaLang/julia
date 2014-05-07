## symbols ##

symbol(s::Symbol) = s
symbol(s::ASCIIString) = symbol(s.data)
symbol(s::UTF8String) = symbol(s.data)
symbol(a::Array{Uint8,1}) =
    ccall(:jl_symbol_n, Any, (Ptr{Uint8}, Int32), a, length(a))::Symbol
symbol(x::Char) = symbol(string(x))

gensym() = ccall(:jl_gensym, Any, ())::Symbol

gensym(s::ASCIIString) = gensym(s.data)
gensym(s::UTF8String) = gensym(s.data)
gensym(a::Array{Uint8,1}) =
    ccall(:jl_tagged_gensym, Any, (Ptr{Uint8}, Int32), a, length(a))::Symbol
gensym(ss::Union(ASCIIString, UTF8String)...) = map(gensym, ss)

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
copy(n::GetfieldNode) = GetfieldNode(n.value, n.name, n.typ)

# copy parts of an AST that the compiler mutates
astcopy(x::Union(SymbolNode,GetfieldNode,Expr)) = copy(x)
astcopy(x::Array{Any,1}) = map(astcopy, x)
astcopy(x) = x

==(x::Expr, y::Expr) = x.head === y.head && x.args == y.args
==(x::QuoteNode, y::QuoteNode) = x.value == y.value

function show(io::IO, tv::TypeVar)
    if !is(tv.lb, None)
        show(io, tv.lb)
        print(io, "<:")
    end
    print(io, tv.name)
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

## some macro utilities ##

find_vars(e) = find_vars(e, {})
function find_vars(e, lst)
    if isa(e,Symbol)
        if !isdefined(e) || isconst(e)
            # exclude global constants
        else
            push!(lst, e)
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
