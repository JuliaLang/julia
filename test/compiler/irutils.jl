using Core.IR
using Core.Compiler: IRCode, IncrementalCompact, singleton_type, VarState
using Base.Meta: isexpr
using InteractiveUtils: gen_call_with_extracted_types_and_kwargs

argextype(@nospecialize args...) = Core.Compiler.argextype(args..., VarState[])
code_typed1(args...; kwargs...) = first(only(code_typed(args...; kwargs...)))::CodeInfo
macro code_typed1(ex0...)
    return gen_call_with_extracted_types_and_kwargs(__module__, :code_typed1, ex0)
end
get_code(args...; kwargs...) = code_typed1(args...; kwargs...).code
macro get_code(ex0...)
    return gen_call_with_extracted_types_and_kwargs(__module__, :get_code, ex0)
end

# check if `x` is a statement with a given `head`
isnew(@nospecialize x) = isexpr(x, :new)
issplatnew(@nospecialize x) = isexpr(x, :splatnew)
isreturn(@nospecialize x) = isa(x, ReturnNode) && isdefined(x, :val)

# check if `x` is a dynamic call of a given function
iscall(y) = @nospecialize(x) -> iscall(y, x)
function iscall((src, f)::Tuple{IR,Base.Callable}, @nospecialize(x)) where IR<:Union{CodeInfo,IRCode,IncrementalCompact}
    return iscall(x) do @nospecialize x
        singleton_type(argextype(x, src)) === f
    end
end
function iscall(pred::Base.Callable, @nospecialize(x))
    if isexpr(x, :(=))
        x = x.args[2]
    end
    return isexpr(x, :call) && pred(x.args[1])
end

# check if `x` is a statically-resolved call of a function whose name is `sym`
isinvoke(y) = @nospecialize(x) -> isinvoke(y, x)
isinvoke(sym::Symbol, @nospecialize(x)) = isinvoke(mi->mi.def.name===sym, x)
isinvoke(pred::Function, @nospecialize(x)) = isexpr(x, :invoke) && pred(x.args[1]::MethodInstance)

fully_eliminated(@nospecialize args...; retval=(@__FILE__), kwargs...) =
    fully_eliminated(code_typed1(args...; kwargs...); retval)
fully_eliminated(src::CodeInfo; retval=(@__FILE__)) = fully_eliminated(src.code; retval)
fully_eliminated(ir::IRCode; retval=(@__FILE__)) = fully_eliminated(ir.stmts.stmt; retval)
function fully_eliminated(code::Vector{Any}; retval=(@__FILE__), kwargs...)
    length(code) == 1 || return false
    retstmt = only(code)
    isreturn(retstmt) || return false
    retval === (@__FILE__) && return true
    retval′ = retstmt.val
    if retval′ isa QuoteNode
        retval′ = retval′.value
    end
    return retval′ == retval
end
macro fully_eliminated(ex0...)
    return gen_call_with_extracted_types_and_kwargs(__module__, :fully_eliminated, ex0)
end
