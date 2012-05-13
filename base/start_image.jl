# set up non-serializable state

const STDIN  = _uv_tty2tty(ccall(:jl_stdin ,Ptr{Void},()))
const STDOUT = _uv_tty2tty(ccall(:jl_stdout,Ptr{Void},()))
const STDERR = _uv_tty2tty(ccall(:jl_stderr,Ptr{Void},()))
OUTPUT_STREAM = STDOUT

# restore shared library handles

# Essential libraries
_jl_libpcre = dlopen("libpcre")
_jl_libgrisu = dlopen("libgrisu")
#_jl_libm = dlopen("libm")
_jl_libfdm = dlopen("libfdm")
_jl_librandom = dlopen("librandom"); _jl_librandom_init();

# Optional libraries
const _jl_libblas = dlopen(_jl_libblas_name)
const _jl_liblapack = (_jl_libblas_name == _jl_liblapack_name) ? _jl_libblas : dlopen(_jl_liblapack_name)
@unix_only begin
    const _jl_libfftw = dlopen("libfftw3_threads")
    const _jl_libfftwf = dlopen("libfftw3f_threads")
end
@windows_only begin
    const _jl_libfftw = dlopen("libfftw3") #On MinGw FFTW must be built with integrated threads
    const _jl_libfftwf = dlopen("libfftw3f")
end

##_jl_libglpk = dlopen("libglpk")
##_jl_libglpk = dlopen("libglpk_wrapper")

# set CPU core count
const CPU_CORES = ccall(:jl_cpu_cores, Int32, ())
