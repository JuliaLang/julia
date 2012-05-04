## symbols ##

symbol(s::ASCIIString) = symbol(s.data)
symbol(s::UTF8String) = symbol(s.data)
symbol(a::Array{Uint8,1}) =
    ccall(:jl_symbol_n, Any, (Ptr{Uint8}, Int32), a, length(a))::Symbol

gensym() = ccall(:jl_gensym, Any, ())::Symbol
gensym(n::Integer) = ntuple(n, i->gensym())

gensym(s::ASCIIString) = gensym(s.data)
gensym(s::UTF8String) = gensym(s.data)
gensym(a::Array{Uint8,1}) =
    ccall(:jl_tagged_gensym, Any, (Ptr{Uint8}, Int32), a, length(a))::Symbol
gensym(ss::Union(ASCIIString, UTF8String)...) = map(gensym, ss)

type UniqueNames
    names::Array{Any,1}
    UniqueNames() = new({})
end

let _names = {}
global gensym
function gensym(u::UniqueNames)
    nu = length(u.names)
    if nu >= length(_names)
        push(_names, gensym())
    end
    s = _names[nu+1]
    push(u.names, s)
    return s
end
end

macro gensym(names...)
    blk = expr(:block)
    for name in names
        push(blk.args, :($name = gensym($(string(name)))))
    end
    push(blk.args, :nothing)
    return blk
end

## expressions ##

expr(hd::Symbol, args::ANY...) = Expr(hd, {args...}, Any)
expr(hd::Symbol, args::Array{Any,1}) = Expr(hd, args, Any)
copy(e::Expr) = Expr(e.head, isempty(e.args) ? e.args : map(copy,e.args), e.typ)
copy(s::SymbolNode) = SymbolNode(s.name, s.typ)

isequal(x::Expr, y::Expr) = (is(x.head,y.head) && isequal(x.args,y.args))
isequal(x::SymbolNode, y::SymbolNode) = is(x.name,y.name)
isequal(x::SymbolNode, y::Symbol)     = is(x.name,y)
isequal(x::Symbol    , y::SymbolNode) = is(x,y.name)

function show(io, tv::TypeVar)
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

## misc syntax ##

macro eval(x)
    :(eval($expr(:quote,x)))
end

macro task(ex)
    :(Task(()->$ex))
end
