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

dlsym(hnd, s::String) = ccall(:jl_dlsym, Ptr{Void}, (Ptr{Void}, Ptr{Uint8}), hnd, s)
dlsym(hnd, s::Symbol) = ccall(:jl_dlsym, Ptr{Void}, (Ptr{Void}, Ptr{Uint8}), hnd, s)
dlsym_e(hnd, s::Union(Symbol,String)) = ccall(:jl_dlsym_e, Ptr{Void}, (Ptr{Void}, Ptr{Uint8}), hnd, s)
dlopen(s::String, flags::Integer) = ccall(:jl_load_dynamic_library, Ptr{Void}, (Ptr{Uint8},Uint32), s, flags)
dlopen(s::String) = dlopen(s, RTLD_LAZY | RTLD_DEEPBIND)
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
