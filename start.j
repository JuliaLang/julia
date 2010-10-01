# core functionality
load("range.j")
load("prim.j")
load("show.j")
load("operators.j")

# primitive data types
load("bool.j")
load("scalar.j")
load("int.j")
load("float.j")

# data types needed by inference
load("array.j")
load("intset.j")
load("table.j")

# compiler
load("inference.j")
(()->ccall(dlsym(JuliaDLHandle,"jl_enable_inference"),Void,()))()

# other primitive data types
load("complex.j")
load("rational.j")
load("string.j")
load("latin1.j")
load("utf8.j")

# other data types
load("list.j")
load("dequeue.j")
load("sparse.j")
load("tree.j")

# external libraries
load("libc.j")
load("math.j")
load("blas.j")
load("lapack.j")

# I/O and concurrency
load("io.j")
load("task.j")
load("multi.j")

# misc
load("util.j")

scheduler = Task(schedule)
# bootstrap the current task into the scheduler.
# this way every future call to the scheduler enters/exits through the
# scheduler's internal "yieldto" call.
make_scheduled(current_task())
yieldto(scheduler)
