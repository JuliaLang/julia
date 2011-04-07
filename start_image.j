# set up non-serializable state
libc = dlopen("libc")
libm = dlopen("libm")

ccall(:jl_set_memio_func, Void, ())
stdout_stream = make_stdout_stream()
set_current_output_stream(stdout_stream)
stdin_stream = fdio(ccall(:jl_stdin, Int32, ()))
stderr_stream = fdio(ccall(:jl_stderr, Int32, ()))

# files not part of core image
# mostly involving external libraries
load("regex.j")

setenv("GOTO_NUM_THREADS", "1")
libBLAS = dlopen("libBLAS")
libLAPACK = dlopen("libLAPACK")

load("custom.j")
