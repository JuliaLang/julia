module JS

using .Base.Meta
import .Base: convert

export @jscall

abstract type JSBoxed end
primitive type JSObject <: JSBoxed 32 end
primitive type JSString <: JSBoxed 32 end
primitive type JSSymbol <: JSBoxed 32 end
struct JSUndefined; end
struct JSNull; end

const JSNumber = Float64

const undefined = JSUndefined.instance
const null = JSNull.instance

const JSAny = Union{JSBoxed, Float64, JSUndefined, JSNull}

macro jscall(expr)
    isexpr(expr, :call) || error("@jscall argument must be a call")
    target_expr = expr.args[1]
    if isa(target_expr, Symbol)
        target = QuoteNode(target_expr)
    else
        if !isexpr(target_expr, :(.)) ||
           !isa(target_expr.args[1], Symbol) ||
           !(isa(target_expr.args[2], QuoteNode) &&
             isa(target_expr.args[2].value, Symbol))
            error("Unsupported call target `$(target_expr)`")
        end
        target = (target_expr.args[1], target_expr.args[2].value)
    end
    b = Expr(:block)
    converted_args = Symbol[]
    for a in expr.args[2:end]
        x = gensym()
        push!(b.args, :($x = jsconvert($(esc(a)))))
        push!(converted_args, x)
    end
    atypes = [JSAny for _ in converted_args]
    quote
        $b
        $(Expr(:foreigncall, target, JSAny, Core.svec(atypes...), QuoteNode(:jscall),
            length(atypes), converted_args...))
    end
end

jsconvert(x) = convert(JSAny, x)

function getproperty(this::JSObject, sym::Symbol)
    @jscall Reflect.get(this, sym)
end

function setproperty!(this::JSObject, sym::Symbol, val)
    @jscall Reflect.set(this, sym, val)
end

# Yes, we're converting a pointer to Float64 here, but at least in
# compiled code, we basically expect the compiler on both sides to
# undo this conversion.
jsconvert(x::Ptr) = convert(JSNumber, convert(Int32, x))
jsconvert(x::AbstractString) = convert(JSString, x)

function convert(::Type{JSString}, x::String)
    @GC.preserve x begin
        ptr = pointer(x)
        @jscall UTF8ToString(ptr)
    end
end

end
