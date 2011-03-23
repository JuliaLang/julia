# set up non-serializable state
libc = dlopen("libc")

ccall(:jl_set_memio_func, Void, ())
stdout_stream = make_stdout_stream()
set_current_output_stream(stdout_stream)
stdin_stream = fdio(ccall(:jl_stdin, Int32, ()))
stderr_stream = fdio(ccall(:jl_stderr, Int32, ()))

# files not part of core image
# mostly involving external libraries
load("regex.j")
load("env.j")

load("math_libm.j")
load("linalg_blas.j")
load("linalg_lapack.j")

load("libc.j")
load("util.j")
