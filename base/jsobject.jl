module JS

using .Base.Meta
import .Base: convert

export @js, JSObject

abstract type JSBoxed end
primitive type JSObject <: JSBoxed 32 end
primitive type JSString <: JSBoxed 32 end
primitive type JSSymbol <: JSBoxed 32 end
primitive type JSFunction <: JSBoxed 32 end
struct JSUndefined; end
struct JSNull; end

const JSNumber = Float64

const undefined = JSUndefined.instance
const null = JSNull.instance

const JSAny = Union{JSBoxed, Float64, JSUndefined, JSNull}

function parse_braces_expr(expr)
    @assert isexpr(expr, :braces)
    ret = Expr(:block)
    s = gensym()
    push!(ret.args, :($s = $JSObject()))
    for arg in expr.args
        isexpr(arg, :call) || return :(error("Unrecognized expression `$(arg.head)`"))
        (arg.args[1] === Symbol(":")) || return :(error("Expected `:`, got $(arg.args[1])"))
        length(arg.args) == 3 || return :(error("Excessive `:`"))
        isa(arg.args[2], Symbol) || return :(error("Expected symbol, got $(arg.args[2])"))
        val = arg.args[3]
        if isexpr(val, :braces)
            val = parse_braces_expr(val)
        else
            val = esc(val)
        end
        push!(ret.args, :(setproperty!($s, $(quot(arg.args[2])), $val)))
    end
    push!(ret.args, s)
    ret
end

function parse_call_expr(expr)
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
        target = (target_expr.args[2].value, target_expr.args[1])
    end
    b = Expr(:block)
    converted_args = Symbol[]
    for a in expr.args[2:end]
        x = gensym()
        if isexpr(a, :braces)
            a = parse_braces_expr(a)
        else
            a = :(jsconvert($(esc(a))))
        end
        push!(b.args, :($x = $a))
        push!(converted_args, x)
    end
    atypes = [:JSAny for _ in converted_args]
    quote
        $b
        $(Expr(:foreigncall, target, :JSAny, Core.svec(atypes...), length(atypes), QuoteNode(:jscall),
            length(atypes), converted_args...))
    end
end

macro js(expr)
    if isexpr(expr, :call)
        return parse_call_expr(expr)
    elseif isexpr(expr, :braces)
        return parse_braces_expr(expr)
    else
        return :(error("Invalid expression for @js macro $(expr.head)"))
    end
end

jsnew(f::JSFunction, args::JSObject) = @js Reflect.construct(f, args)

jsconvert(x) = convert(JSAny, x)

function Base.getproperty(this::JSObject, sym::Symbol)
    @js Reflect.get(this, sym)
end

function Base.setproperty!(this::JSObject, sym::Symbol, val)
    @js Reflect.set(this, sym, val)
end

# Yes, we're converting a pointer to Float64 here, but at least in
# compiled code, we basically expect the compiler on both sides to
# undo this conversion.
jsconvert(x::Ptr) = convert(JSNumber, convert(UInt32, x))
jsconvert(x::AbstractString) = convert(JSString, x)
jsconvert(x::Symbol) = jsconvert(string(x))

function convert(::Type{JSString}, x::String)
    @GC.preserve x begin
        ptr = pointer(x)
        @js UTF8ToString(ptr)
    end
end

global Object = nothing
global EmptyArray = nothing
function __init__()
    global Object, EmptyArray
    # TODO: Without the eval, this gets codegen'ed into the system image,
    # where we don't yet support this calling convention.
    Core.eval(JS, quote
        @js Module.initialize_jscall_runtime()
        Object = @js eval("Object")
        EmptyArray = @js Array(0.0)
    end)
end

JSObject() = jsnew(Object, EmptyArray)::JSObject

end
