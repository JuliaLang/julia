# set up non-serializable state
libc = dlopen("libc")
libm = dlopen("libm")
libfdm = dlopen("libfdm")

ccall(:jl_set_memio_func, Void, ())
stdout_stream = make_stdout_stream()
set_current_output_stream(stdout_stream)
stdin_stream = fdio(ccall(:jl_stdin, Int32, ()))
stderr_stream = fdio(ccall(:jl_stderr, Int32, ()))

# restore shared library handles

libpcre = dlopen("libpcre")

libBLAS = dlopen("libLAPACK")
libLAPACK = libBLAS

libarpack = dlopen("libarpack")

try
    load(strcat(getcwd(),"/custom.j"))
catch
end
