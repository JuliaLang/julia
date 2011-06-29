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
load("complex.j")
load("rational.j")

# core data structures (used by type inference)
load("tensor.j")
load("array.j")
load("intset.j")
load("table.j")

# compiler
load("inference.j")
ccall(:jl_enable_inference,Void,())

# load libc - julia already links against it so process handle works
libc = ccall(:jl_load_dynamic_library, Ptr{Void}, (Ptr{Uint8},), C_NULL)

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
load("random.j")
load("combinatorics.j")
load("linalg.j")
load("linalg_blas.j")
load("linalg_lapack.j")
load("linalg_arpack.j")
load("fft.j")

# additional data types
#load("list.j")
#load("queue.j")
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
load("regex.j")

# prime method cache with some things we know we'll need right after startup
length(1:2:3)
(HashTable(0)[1])=()->()
numel(intset())
has(intset(),2)
del_all(FDSet())
start(HashTable(0))
done(HashTable(0),0)
get(HashTable(0), 0, ())
add(FDSet(),0)
2==2.0
has(FDSet(),0)
isequal(2,2)

compile_hint(getcwd, ())
compile_hint(fdio, (Int32,))
compile_hint(ProcessGroup, (Int32, Array{Any,1}, Array{Any,1}))
compile_hint(select_read, (FDSet, Int32))
compile_hint(next, (HashTable{Any,Any}, Int32))
compile_hint(perform_work, ())
compile_hint(isempty, (Array{Any,1},))
compile_hint(ref, (HashTable{Any,Any}, Int32))
compile_hint(event_loop, (Bool,))
compile_hint(start_client, ())

ccall(:jl_save_system_image, Void, (Ptr{Uint8},Ptr{Uint8}),
      cstring("sys.ji"), cstring("j/start_image.j"))
