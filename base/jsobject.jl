module JS

using .Base.Meta
import .Base: convert

export @js, JSObject, @js_str

abstract type JSBoxed end
abstract type JSAnyObject <: JSBoxed; end
primitive type JSFunction <: JSAnyObject 32 end
primitive type JSString <: JSBoxed 32 end
primitive type JSSymbol <: JSBoxed 32 end
struct JSUndefined; end
struct JSNull; end

show(io::IO, f::JSFunction) = print(io, "JSFunction(", reinterpret(UInt32, f), ")")

struct JSBoundFunction
    this::JSAnyObject
    f::JSFunction
end

const JSNumber = Float64

const undefined = JSUndefined.instance
const null = JSNull.instance

const JSAny = Union{JSBoxed, Float64, JSUndefined, JSNull}

function parse_braces_expr(expr)
    @assert isexpr(expr, :braces)
    ret = Expr(:block)
    s = gensym()
    push!(ret.args, :($s = $JSAnyObject()))
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

function parse_call_expr(expr; cc = :jscall)
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
        $(Expr(:foreigncall, target, :JSAny, Core.svec(atypes...), length(atypes), QuoteNode(cc), converted_args...))
    end
end

macro js(args...)
    if args[1] == :await
        length(args) == 1 || :(error("`@js await` expects only one additional argument"))
        expr = args[2]
        isexpr(expr, :call) || :(error("`@js await` expects a call"))
        call = parse_call_expr(expr)
        return quote
            promise = $call
            wait(promise)
        end
    elseif args[1] == :new
        length(args) == 1 || :(error("`@js new` expects only one additional argument"))
        expr = args[2]
        isexpr(expr, :call) || :(error("`@js new` expects a call"))
        return parse_call_expr(expr; cc=:jsnew)
    else
        length(args) == 1 || :(error("@js macro expects only one argument"))
        expr = args[1]
    end
    if isexpr(expr, :call)
        return parse_call_expr(expr)
    elseif isexpr(expr, :braces)
        return parse_braces_expr(expr)
    else
        return :(error(string("Invalid expression for @js macro", $(expr.head))))
    end
end

macro js_str(str)
    :(@js eval($str))
end

jsnew(f::JSFunction, args) = @js Reflect.construct(f, args)

jsconvert(x) = isa(typeof(x), JSFunction) || isa(x, JSAny) ? x : convert(JSBoxed, x)

function Base.getproperty(this::JSAnyObject, sym::Symbol)
    result = @js Reflect.get(this, sym)
    if isa(result, JSFunction)
        return JSBoundFunction(this, result)
    end
    return result
end

function Base.setproperty!(this::JSAnyObject, sym::Symbol, val)
    @js Reflect.set(this, sym, val)
end

@eval function (f::JSBoundFunction)(args...)
    ff = f.f
    this = f.this
    args = map(jsconvert, args)
    $(Expr(:splatforeigncall, :ff, JSAny, Core.svec(JSAny, Vararg{JSAny}), 1, QuoteNode(:jscall), 2, Expr(:tuple, :this), :args))
end

# Yes, we're converting a pointer to Float64 here, but at least in
# compiled code, we basically expect the compiler on both sides to
# undo this conversion.
jsconvert(x::Ptr) = convert(JSNumber, convert(UInt32, x))
jsconvert(x::AbstractString) = convert(JSString, x)
jsconvert(x::Symbol) = jsconvert(string(x))
jsconvert(x::Integer) = convert(Float64, x)
# This isn't too hard, use Function.prototype.bind()
jsconvert(x::JSBoundFunction) = error("Not implemented yet.")

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
    ccall(:jl_set_jsfunction_type, Cvoid, (Any,Any), JSFunction, JSAnyObject)
    # TODO: Without the eval, this gets codegen'ed into the system image,
    # where we don't yet support this calling convention.
    Core.eval(JS, quote
        @js Module.initialize_jscall_runtime()
        Object = @js eval("Object")
        EmptyArray = @js Array(0.0)
        function Base.wait(promise::js"Promise")
            @js enq_wait_promise(promise)
            wait()
        end
        function Base.convert(::Type{Vector{UInt8}}, buf::js"ArrayBuffer")
            jlbuf = Vector{UInt8}(undef, Int(buf.byteLength))
            js"HEAPU8".set((@js new Uint8Array(buf)), convert(Int, pointer(jlbuf)))
            jlbuf
        end
    end)
end

JSAnyObject() = jsnew(Object, EmptyArray)::Object

end
