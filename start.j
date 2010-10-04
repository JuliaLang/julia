# core operations & types
load("bitops.j")
load("range.j")
load("tuple.j")
load("cell.j")
load("expr.j")

# core functionality
load("convert.j")
load("promote.j")
load("int.j")
load("float.j")
load("generic.j")
load("reduce.j")
load("array.j")
load("intset.j")
load("table.j")

# compiler
load("inference.j")
(()->ccall(dlsym(JuliaDLHandle,"jl_enable_inference"),Void,()))()

# rational & complex
load("rational.j")
load("complex.j")

# strings & printing
load("string.j")
load("latin1.j")
load("utf8.j")
load("show.j")

# additional data types
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
