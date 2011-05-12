# core operations & types
load("range.j")
load("tuple.j")
load("cell.j")
load("expr.j")
load("error.j")

# core numeric operations & types
load("bool.j")
load("number.j")
load("int.j")
load("float.j")
load("pointer.j")
load("char.j")
load("operators.j")
load("reduce.j")

# core data structures (used by type inference)
load("tensor.j")
load("array.j")
load("intset.j")
load("table.j")

# compiler
load("inference.j")
ccall(:jl_enable_inference,Void,())

# load libc
libc = dlopen("libc")

# io, strings, printing & regexen
load("io.j")
ccall(:jl_set_memio_func, Void, ())
stdout_stream = make_stdout_stream()
set_current_output_stream(stdout_stream)
stdin_stream = fdio(ccall(:jl_stdin, Int32, ()))
stderr_stream = fdio(ccall(:jl_stderr, Int32, ()))
load("string.j")
load("ascii.j")
load("utf8.j")
load("show.j")
load("regex.j")
load("env.j")

# core math functions
load("intfuncs.j")
load("floatfuncs.j")
load("math.j")
load("math_libm.j")
load("combinatorics.j")
load("linalg.j")
load("linalg_blas.j")
load("linalg_lapack.j")

# additional data types
load("complex.j")
load("rational.j")
load("list.j")
load("queue.j")
load("sparse.j")
load("tree.j")
load("set.j")

# concurrency & ipc
load("iterator.j")
load("task.j")
load("process.j")
load("serialize.j")
load("multi.j")
load("darray.j")

# misc
load("libc.j")
load("util.j")
load("repl.j")

print_banner()
