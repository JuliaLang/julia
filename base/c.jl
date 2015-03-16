# definitions related to C interface

import Core.Intrinsics.cglobal

cfunction(f::Function, r, a) = ccall(:jl_function_ptr, Ptr{Void}, (Any, Any, Any), f, r, a)

if ccall(:jl_is_char_signed, Any, ())
    typealias Cchar Int8
else
    typealias Cchar UInt8
end
typealias Cuchar UInt8
typealias Cshort Int16
typealias Cushort UInt16
typealias Cint Int32
typealias Cuint UInt32
if OS_NAME === :Windows
    typealias Clong Int32
    typealias Culong UInt32
    typealias Cwchar_t UInt16
else
    typealias Clong Int
    typealias Culong UInt
    typealias Cwchar_t Int32
end
typealias Cptrdiff_t Int
typealias Csize_t UInt
typealias Cssize_t Int
typealias Cintmax_t Int64
typealias Cuintmax_t UInt64
typealias Clonglong Int64
typealias Culonglong UInt64
typealias Cfloat Float32
typealias Cdouble Float64

const sizeof_off_t = ccall(:jl_sizeof_off_t, Cint, ())

if sizeof_off_t === 4
    typealias FileOffset Int32
else
    typealias FileOffset Int64
end

typealias Coff_t FileOffset

# deferring (or un-deferring) ctrl-c handler for external C code that
# is not interrupt safe (see also issue #2622).  The sigatomic_begin/end
# functions should always be called in matched pairs, ideally via:
#            disable_sigint() do .. end
# reennable_sigint is provided so that immediate ctrl-c handling is
# re-enabled within a sigatomic region, e.g. inside a Julia callback function
# within a long-running C routine.
sigatomic_begin() = ccall(:jl_sigatomic_begin, Void, ())
sigatomic_end() = ccall(:jl_sigatomic_end, Void, ())
disable_sigint(f::Function) = try sigatomic_begin(); f(); finally sigatomic_end(); end
reenable_sigint(f::Function) = try sigatomic_end(); f(); finally sigatomic_begin(); end

function ccallable(f::Function, rt::Type, argt::(Type...), name::Union(AbstractString,Symbol)=string(f))
    ccall(:jl_extern_c, Void, (Any, Any, Any, Ptr{UInt8}), f, rt, argt, name)
end

function ccallable(f::Function, argt::(Type...), name::Union(AbstractString,Symbol)=string(f))
    ccall(:jl_extern_c, Void, (Any, Ptr{Void}, Any, Ptr{UInt8}), f, C_NULL, argt, name)
end

macro ccallable(def)
    if isa(def,Expr) && (def.head === :(=) || def.head === :function)
        sig = def.args[1]
        if sig.head === :call
            name = sig.args[1]
            at = map(sig.args[2:end]) do a
                if isa(a,Expr) && a.head === :(::)
                    a.args[2]
                else
                    :Any
                end
            end
            return quote
                $(esc(def))
                ccallable($(esc(name)), $(Expr(:tuple, map(esc, at)...)))
            end
        end
    end
    error("expected method definition in @ccallable")
end
