# definitions related to C interface

import Core.Intrinsics.cglobal

# constants to match JL_RTLD_* in src/julia.h
const RTLD_LOCAL     = 0x00000000
const RTLD_GLOBAL    = 0x00000001
const RTLD_LAZY      = 0x00000002
const RTLD_NOW       = 0x00000004
const RTLD_NODELETE  = 0x00000008
const RTLD_NOLOAD    = 0x00000010
const RTLD_DEEPBIND  = 0x00000020
const RTLD_FIRST     = 0x00000040

function dlsym(hnd::Ptr, s::Union(Symbol,AbstractString))
    hnd == C_NULL && error("NULL library handle")
    ccall(:jl_dlsym, Ptr{Void}, (Ptr{Void}, Ptr{UInt8}), hnd, s)
end

function dlsym_e(hnd::Ptr, s::Union(Symbol,AbstractString))
    hnd == C_NULL && error("NULL library handle")
    ccall(:jl_dlsym_e, Ptr{Void}, (Ptr{Void}, Ptr{UInt8}), hnd, s)
end

dlopen(s::Symbol, flags::Integer = RTLD_LAZY | RTLD_DEEPBIND) =
    dlopen(string(s), flags)

dlopen(s::AbstractString, flags::Integer = RTLD_LAZY | RTLD_DEEPBIND) =
    ccall(:jl_load_dynamic_library, Ptr{Void}, (Ptr{UInt8},UInt32), s, flags)

dlopen_e(s::AbstractString, flags::Integer = RTLD_LAZY | RTLD_DEEPBIND) =
    ccall(:jl_load_dynamic_library_e, Ptr{Void}, (Ptr{UInt8},UInt32), s, flags)

dlopen_e(s::Symbol, flags::Integer = RTLD_LAZY | RTLD_DEEPBIND) =
    dlopen_e(string(s), flags)

dlclose(p::Ptr) = if p!=C_NULL; ccall(:uv_dlclose,Void,(Ptr{Void},),p); end

cfunction(f::Function, r, a) =
    ccall(:jl_function_ptr, Ptr{Void}, (Any, Any, Any), f, r, a)

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

# flush C stdio output from external libraries
flush_cstdio() = ccall(:jl_flush_cstdio, Void, ())

function find_library{T<:ByteString, S<:ByteString}(libnames::Array{T,1}, extrapaths::Array{S,1}=ASCIIString[])
    for lib in libnames
        for path in extrapaths
            l = joinpath(path, lib)
            p = dlopen_e(l, RTLD_LAZY)
            if p != C_NULL
                dlclose(p)
                return l
            end
        end
        p = dlopen_e(lib, RTLD_LAZY)
        if p != C_NULL
            dlclose(p)
            return lib
        end
    end
    return ""
end

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
