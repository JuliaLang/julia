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

# strings & printing
load("io.j")
ccall(:jl_set_memio_func, Void, ())
set_current_output_stream(make_stdout_stream())  # for error reporting
load("string.j")
load("ascii.j")
load("utf8.j")
load("show.j")

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
load("linalg_arpack.j")

# additional data types
load("complex.j")
load("rational.j")
#load("list.j")
load("queue.j")
load("sparse.j")
#load("tree.j")
load("set.j")

# I/O and concurrency
load("iterator.j")
load("task.j")
load("process.j")
load("serialize.j")
load("multi.j")
load("darray.j")

# misc
load("libc.j")
load("util.j")

ccall(:jl_save_system_image, Void, (Ptr{Uint8},Ptr{Uint8}),
      cstring("sys.ji"), cstring("j/start_image.j"))
