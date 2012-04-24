# set up non-serializable state

const stdout_stream = make_stdout_stream()
OUTPUT_STREAM = stdout_stream
const stdin_stream = make_stdin_stream()
const stderr_stream = make_stderr_stream()

# restore shared library handles

# Essential libraries
_jl_libpcre = dlopen("libpcre")
_jl_libgrisu = dlopen("libgrisu")
_jl_libm = dlopen("libm")
_jl_libfdm = dlopen("libfdm")
_jl_librandom = dlopen("librandom"); _jl_librandom_init();

# Optional libraries
_jl_libblas = dlopen("liblapack")
_jl_liblapack = _jl_libblas
_jl_libfftw = dlopen("libfftw3")
_jl_libfftwf = dlopen("libfftw3f")

##_jl_libglpk = dlopen("libglpk")
##_jl_libglpk = dlopen("libglpk_wrapper")
