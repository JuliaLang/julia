# set up non-serializable state

# restore shared library handles
_jl_lib = ccall(:malloc, Ptr{Void}, (Uint,), 
        ccall(:jl_sizeof_uv_lib_t, Uint, ()))
@assert ccall(:jl_uv_dlopen, Int32, (Ptr{Void}, Ptr{Void}), C_NULL, _jl_lib) == 0

@unix_only _jl_repl = _jl_lib
@windows_only _jl_repl = ccall(:GetModuleHandleA,stdcall,Ptr{Void},(Ptr{Void},),C_NULL)

# Essential libraries
libpcre = dlopen("libpcre")
libgrisu = dlopen("libgrisu")
_jl_libm = dlopen("libm")
_jl_libfdm = dlopen("libfdm")
_jl_librandom = dlopen("librandom");
@windows_only _jl_advapi32 = dlopen("Advapi32")

# Optional libraries
const _jl_libblas = dlopen(_jl_libblas_name)
const _jl_liblapack = (_jl_libblas_name == _jl_liblapack_name) ? _jl_libblas : dlopen(_jl_liblapack_name)
const libfftw = dlopen("libfftw3_threads")
const libfftwf = dlopen("libfftw3f_threads")

##_jl_libglpk = dlopen("libglpk")
##_jl_libglpk = dlopen("libglpk_wrapper")
