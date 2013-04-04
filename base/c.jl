# definitions related to C interface

ptr_arg_convert{T}(::Type{Ptr{T}}, x) = convert(T, x)
ptr_arg_convert(::Type{Ptr{Void}}, x) = x

# conversion used by ccall
cconvert(T, x) = convert(T, x)
# use the code in ccall.cpp to safely allocate temporary pointer arrays
cconvert{T}(::Type{Ptr{Ptr{T}}}, a::Array) = a
# TODO: for some reason this causes a strange type inference problem
#cconvert(::Type{Ptr{Uint8}}, s::String) = bytestring(s)

# constants to match JL_RTLD_* in src/julia.h
const RTLD_LOCAL     = 0x00000000
const RTLD_GLOBAL    = 0x00000001
const RTLD_LAZY      = 0x00000002
const RTLD_NOW       = 0x00000004
const RTLD_NODELETE  = 0x00000008
const RTLD_NOLOAD    = 0x00000010
const RTLD_DEEPBIND  = 0x00000020
const RTLD_FIRST     = 0x00000040

dlsym(hnd, s::Union(Symbol,String)) = ccall(:jl_dlsym, Ptr{Void}, (Ptr{Void}, Ptr{Uint8}), hnd, s)
dlsym_e(hnd, s::Union(Symbol,String)) = ccall(:jl_dlsym_e, Ptr{Void}, (Ptr{Void}, Ptr{Uint8}), hnd, s)
dlopen(s::String, flags::Integer) = ccall(:jl_load_dynamic_library, Ptr{Void}, (Ptr{Uint8},Uint32), s, flags)
dlopen_e(s::String, flags::Integer) = ccall(:jl_load_dynamic_library_e, Ptr{Void}, (Ptr{Uint8},Uint32), s, flags)
dlopen(s::String) = dlopen(s, RTLD_LAZY | RTLD_DEEPBIND)
dlopen_e(s::String) = dlopen_e(s, RTLD_LAZY | RTLD_DEEPBIND)
dlclose(p::Ptr) = ccall(:uv_dlclose,Void,(Ptr{Void},),p)

cfunction(f::Function, r, a) =
    ccall(:jl_function_ptr, Ptr{Void}, (Any, Any, Any), f, r, a)

if ccall(:jl_is_char_signed, Any, ())
    typealias Cchar Int8
else
    typealias Cchar Uint8
end
typealias Cuchar Uint8
typealias Cshort Int16
typealias Cushort Uint16
typealias Cint Int32
typealias Cuint Uint32
if OS_NAME === :Windows
    typealias Clong Int32
    typealias Culong Uint32
else
    typealias Clong Int
    typealias Culong Uint
end
typealias Cptrdiff_t Int
typealias Csize_t Uint
typealias Clonglong Int64
typealias Culonglong Uint64
typealias Cfloat Float32
typealias Cdouble Float64
#typealias Ccomplex_float Complex64
#typealias Ccomplex_double Complex128

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
