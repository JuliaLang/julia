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

# load libc - julia already links against it so process handle works
libc = ccall(:jl_load_dynamic_library, Ptr{Void}, (Ptr{Uint8},), C_NULL)
load("libc.j")

# core data structures (used by type inference)
load("tensor.j")
load("array.j")
load("intset.j")
load("table.j")

# compiler
load("inference.j")
ccall(:jl_enable_inference,Void,())

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
load("util.j")
load("regex.j")

# front end
load("client.j")

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
compile_hint(start, (HashTable{Any,Any},))
compile_hint(perform_work, ())
compile_hint(isempty, (Array{Any,1},))
compile_hint(ref, (HashTable{Any,Any}, Int32))
compile_hint(event_loop, (Bool,))
compile_hint(_start, ())
compile_hint(color_available, ())
compile_hint(process_options, (Array{Any,1},))
compile_hint(run_repl, ())
compile_hint(anyp, (Function, Array{Any,1}))
compile_hint(HashTable, (Int32,))
compile_hint(Set, ())
compile_hint(assign, (HashTable{Any,Any}, Bool, Cmd))
compile_hint(rehash, (HashTable{Any,Any}, Int32))
compile_hint(run, (Cmd,))
compile_hint(spawn, (Cmd,))
compile_hint(assign, (HashTable{Any,Any}, Bool, FileDes))
compile_hint(wait, (Int32,))
compile_hint(system_error, (ASCIIString, Bool))
compile_hint(SystemError, (ASCIIString,))
compile_hint(cstring, (SubString,))
compile_hint(has, (EnvHash, ASCIIString))
compile_hint(parse_input_line, (ASCIIString,))
compile_hint(cmp, (Int32, Int32))
compile_hint(min, (Int32, Int32))
compile_hint(==, (ASCIIString, ASCIIString))
compile_hint(arg_gen, (SubString,))
compile_hint(print, (SubString,))

ccall(:jl_save_system_image, Void, (Ptr{Uint8},Ptr{Uint8}),
      cstring("sys.ji"), cstring("j/start_image.j"))
