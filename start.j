# core functionality
load("range.j")
load("prim.j")
load("print.j")
load("util.j")
load("operators.j")

# primitive data types
load("bool.j")
load("scalar.j")
load("int.j")
load("float.j")
load("complex.j")
load("rational.j")
load("string.j")

# complex data types
load("list.j")
load("dequeue.j")
load("intset.j")
load("table.j")
load("array.j")
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

# compiler
load("inference.j")

scheduler = Task(schedule)
# bootstrap the current task into the scheduler.
# this way every future call to the scheduler enters/exits through the
# scheduler's internal "yieldto" call.
make_scheduled(current_task())
yieldto(scheduler)
