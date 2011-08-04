# set up non-serializable state

ccall(:jl_set_memio_func, Void, ())
stdout_stream = make_stdout_stream()
set_current_output_stream(stdout_stream)
stdin_stream = fdio(ccall(:jl_stdin, Int32, ()))
stderr_stream = fdio(ccall(:jl_stderr, Int32, ()))

# restore shared library handles

libc = ccall(:jl_load_dynamic_library, Ptr{Void}, (Ptr{Uint8},), C_NULL)
libm = dlopen("libm")
libfdm = dlopen("libfdm")
libmt = dlopen("libMT"); mt_init();
libpcre = dlopen("libpcre")
libBLAS = dlopen("libLAPACK")
libLAPACK = libBLAS
libarpack = dlopen("libarpack")
libfftw = dlopen("libfftw3")
libfftwf = dlopen("libfftw3f")
