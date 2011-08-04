## symbols ##

symbol(s::ASCIIString) = symbol(s.data)
symbol(s::UTF8String) = symbol(s.data)
symbol(a::Array{Uint8,1}) =
    ccall(:jl_symbol_n, Any, (Ptr{Uint8}, Int32), a, int32(length(a)))::Symbol

gensym() = ccall(:jl_gensym, Any, ())::Symbol

## expressions ##

expr(hd::Symbol, args::ANY...) = Expr(hd, {args...}, Any)
expr(hd::Symbol, args::Array{Any,1}) = Expr(hd, args, Any)
copy(e::Expr) = Expr(e.head, copy(e.args), e.type)
copy(s::SymbolNode) = SymbolNode(s.name, s.type)

isequal(x::Expr, y::Expr) = (is(x.head,y.head) && isequal(x.args,y.args))
isequal(x::SymbolNode, y::SymbolNode) = is(x.name,y.name)

## misc syntax ##

macro eval(x)
    :(eval($expr(:quote,x)))
end
