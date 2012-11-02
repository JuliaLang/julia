# set up non-serializable state

# restore shared library handles
_jl_lib = ccall(:jl_load_dynamic_library,Ptr{Void},(Ptr{None},),C_NULL)
@unix_only _jl_repl = _jl_lib
@windows_only _jl_repl = ccall(:GetModuleHandleA,stdcall,Ptr{Void},(Ptr{Void},),C_NULL)

# Essential libraries
libpcre = dlopen("libpcre")
libgrisu = dlopen("libgrisu")
librandom = dlopen("librandom")
libopenlibm = dlopen("libopenlibm")
@windows_only _jl_advapi32 = dlopen("Advapi32")

# Optional libraries
const libblas = dlopen(libblas_name)
const liblapack = (libblas_name == liblapack_name) ? libblas : dlopen(liblapack_name)
const libfftw = dlopen("libfftw3_threads")
const libfftwf = dlopen("libfftw3f_threads")

##_jl_libglpk = dlopen("libglpk")
##_jl_libglpk = dlopen("libglpk_wrapper")
