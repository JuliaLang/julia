## symbols ##

symbol(s::ASCIIString) = symbol(s.data)
symbol(s::UTF8String) = symbol(s.data)
symbol(a::Array{Uint8,1}) =
    ccall(:jl_symbol_n, Any, (Ptr{Uint8}, Int32), a, length(a))::Symbol

gensym() = ccall(:jl_gensym, Any, ())::Symbol
gensym(n::Integer) = ntuple(n, i->gensym())

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

## expressions ##

expr(hd::Symbol, args::ANY...) = Expr(hd, {args...}, Any)
expr(hd::Symbol, args::Array{Any,1}) = Expr(hd, args, Any)
copy(e::Expr) = Expr(e.head, isempty(e.args) ? e.args : copy(e.args), e.typ)
copy(s::SymbolNode) = SymbolNode(s.name, s.typ)

isequal(x::Expr, y::Expr) = (is(x.head,y.head) && isequal(x.args,y.args))
isequal(x::SymbolNode, y::SymbolNode) = is(x.name,y.name)
isequal(x::SymbolNode, y::Symbol)     = is(x.name,y)
isequal(x::Symbol    , y::SymbolNode) = is(x,y.name)

function show(tv::TypeVar)
    if !is(tv.lb, None)
        show(tv.lb)
        print("<:".data)
    end
    print(tv.name)
    if !is(tv.ub, Any)
        print("<:".data)
        show(tv.ub)
    end
end

## misc syntax ##

macro eval(x)
    :(eval($expr(:quote,x)))
end

macro task(ex)
    :(Task(()->$ex))
end
