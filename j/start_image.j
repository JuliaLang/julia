# set up non-serializable state

const stdout_stream = make_stdout_stream()
set_current_output_stream(stdout_stream)
const stdin_stream = fdio(ccall(:jl_stdin, Int32, ()))
const stderr_stream = fdio(ccall(:jl_stderr, Int32, ()))

# restore shared library handles

libc = ccall(:jl_load_dynamic_library, Ptr{Void}, (Ptr{Uint8},), C_NULL)
libm = dlopen("libm")
libfdm = dlopen("libfdm")
_jl_librandom = dlopen("librandom"); _jl_librandom_init();
libpcre = dlopen("libpcre")
_jl_libBLAS = dlopen("libLAPACK")
_jl_libLAPACK = _jl_libBLAS
_jl_libarpack = dlopen("libarpack")
_jl_libfftw = dlopen("libfftw3")
_jl_libfftwf = dlopen("libfftw3f")
