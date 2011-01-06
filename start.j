# core operations & types
load("range.j")
load("tuple.j")
load("cell.j")
load("expr.j")
load("error.j")

# core numeric operations & types
load("operators.j")
load("reduce.j")
load("bool.j")
load("int.j")
load("float.j")
load("char.j")
load("number.j")
load("pointer.j")

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

# io, strings & printing
load("io.j")
load("string.j")
load("latin1.j")
load("utf8.j")
load("show.j")
load("pcre.j")

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
load("rational.j")
load("complex.j")
load("list.j")
load("dequeue.j")
load("sparse.j")
load("tree.j")
load("set.j")

# concurrency & ipc
stdout_stream = make_stdout_stream()
set_current_output_stream(stdout_stream)
load("task.j")
load("multi.j")
load("process.j")

# misc
load("libc.j")
load("util.j")

scheduler = Task(schedule)
# bootstrap the current task into the scheduler.
# this way every future call to the scheduler enters/exits through the
# scheduler's internal "yieldto" call.
make_scheduled(current_task())
yieldto(scheduler)
