load("base.j")

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
load("promotion.j")
load("reduce.j")
load("complex.j")
load("rational.j")

# core data structures (used by type inference)
load("abstractarray.j")
load("subarray.j")
load("array.j")
load("intset.j")
load("table.j")
load("set.j")

# compiler
load("inference.j")

# I/O, strings & printing
load("io.j")
#set_current_output_stream(make_stdout_stream()) # for error reporting
load("string.j")
load("ascii.j")
load("utf8.j")
load("regex.j")
load("show.j")
load("grisu.j")
load("printf.j")

# system & environment
load("libc.j")
load("env.j")
load("errno_h.j")

# concurrency and parallelism
load("iterator.j")
load("task.j")
load("process.j")
load("serialize.j")
load("multi.j")

# front end
load("client.j")

# core math functions
load("intfuncs.j")
load("floatfuncs.j")
load("math.j")
load("math_libm.j")
load("sort.j")
load("combinatorics.j")
load("statistics.j")

# random number generation
load("random.j")

# sparse matrices
load("sparse.j")

# distributed arrays
load("darray.j")

# utilities - version, timing, help, edit
load("version.j")
load("util.j")
load("datafmt.j")

## External libraries

# linear algebra
load("linalg.j")
load("linalg_blas.j")
load("linalg_lapack.j")
load("linalg_arpack.j")
load("linalg_suitesparse.j")

# signal processing
load("signal.j")
load("signal_fftw.j")


# prime method cache with some things we know we'll need right after startup
length(1:2:3)
(HashTable(0)[1])=()->()
numel(intset())
has(intset(),2)
del_all(FDSet())
start(HashTable(0))
done(HashTable(0),0)
get(HashTable(0), 0, ())
add(FDSet(),int32(0))
2==2.0
2.0==2.0
has(FDSet(),0)
isequal(int32(2),int32(2))
isequal(int64(2),int64(2))

compile_hint(getcwd, ())
compile_hint(fdio, (Int32,))
compile_hint(ProcessGroup, (Int, Array{Any,1}, Array{Any,1}))
compile_hint(select_read, (FDSet, Float64))
compile_hint(next, (HashTable{Any,Any}, Int))
compile_hint(next, (HashTable{Any,Any}, Int32))
compile_hint(start, (HashTable{Any,Any},))
compile_hint(perform_work, ())
compile_hint(isempty, (Array{Any,1},))
compile_hint(isempty, (Array{WorkItem,1},))
compile_hint(ref, (HashTable{Any,Any}, Int32))
compile_hint(event_loop, (Bool,))
compile_hint(_start, ())
compile_hint(_jl_color_available, ())
compile_hint(process_options, (Array{Any,1},))
compile_hint(run_repl, ())
compile_hint(anyp, (Any-->Any, Array{Any,1}))
compile_hint(HashTable, (Int,))
compile_hint(HashTable{Any,Any}, (Int,))
compile_hint(Set, ())
compile_hint(assign, (HashTable{Any,Any}, Bool, Cmd))
compile_hint(rehash, (HashTable{Any,Any}, Int))
compile_hint(run, (Cmd,))
compile_hint(spawn, (Cmd,))
compile_hint(assign, (HashTable{Any,Any}, Bool, FileDes))
compile_hint(wait, (Int32,))
compile_hint(system_error, (ASCIIString, Bool))
compile_hint(SystemError, (ASCIIString,))
compile_hint(has, (EnvHash, ASCIIString))
compile_hint(parse_input_line, (ASCIIString,))
compile_hint(cmp, (Int32, Int32))
compile_hint(min, (Int32, Int32))
compile_hint(==, (ASCIIString, ASCIIString))
compile_hint(arg_gen, (ASCIIString,))
compile_hint(_jl_librandom_init, ())
compile_hint(srand, (ASCIIString, Int))
compile_hint(open, (ASCIIString, Bool, Bool, Bool, Bool))
compile_hint(srand, (Uint64,))
compile_hint(^, (Float64, Int))
compile_hint(done, (IntSet, Int64))
compile_hint(next, (IntSet, Int64))
compile_hint(<, (Int32, Int64))
compile_hint(ht_keyindex, (HashTable{Any,Any}, Int32))
compile_hint(perform_work, (WorkItem,))
compile_hint(notify_done, (WorkItem,))
compile_hint(work_result, (WorkItem,))
compile_hint(del_fd_handler, (Int32,))
compile_hint(enqueue, (Array{WorkItem,1}, WorkItem))
compile_hint(enq_work, (WorkItem,))
compile_hint(pop, (Array{WorkItem,1},))
compile_hint(string, (Int,))
compile_hint(parse_int, (Type{Int64}, ASCIIString, Int))
compile_hint(parse_int, (Type{Int32}, ASCIIString, Int))
compile_hint(repeat, (ASCIIString, Int))
compile_hint(+, ())
compile_hint(KeyError, (Int,))
compile_hint(show, (Float64,))
compile_hint(match, (Regex, ASCIIString))
compile_hint(strlen, (ASCIIString,))
compile_hint(dims2string, (Tuple,))
compile_hint(alignment, (Float64,))
compile_hint(repl_callback, (Expr, Int32))
compile_hint(istaskdone, (Task,))
