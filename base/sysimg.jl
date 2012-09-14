module Base

include("export.jl")

if false
    # simple print definitions for debugging. enable these if something
    # goes wrong during bootstrap before printing code is available.
    length(a::Array) = arraylen(a)
    print(x) = print(stdout_stream, x)
    show(x) = show(stdout_stream, x)
    write(io, a::Array{Uint8,1}) =
        ccall(:ios_write, Uint, (Ptr{Void}, Ptr{Void}, Uint),
              io.ios, a, length(a))
    print(io, s::Symbol) = ccall(:jl_print_symbol, Void, (Ptr{Void},Any,),
                                 io.ios, s)
    print(io, s::ASCIIString) = (write(io, s.data);nothing)
    print(io, x) = show(io, x)
    println(io, x) = (print(io, x); print(io, "\n"))
    show(io, x) = ccall(:jl_show_any, Void, (Any, Any,), io, x)
    show(io, s::ASCIIString) = (write(io, s.data);nothing)
    show(io, s::Symbol) = print(io, s)
    show(io, b::Bool) = print(io, b ? "true" : "false")
    show(io, n::Int64) = ccall(:jl_print_int64, Void, (Ptr{Void}, Int64,), io, n)
    show(io, n::Integer)  = show(io, int64(n))
    print(io, a...) = for x=a; print(io, x); end
    function show(io, e::Expr)
        print(io, e.head)
        print(io, "(")
        for i=1:arraylen(e.args)
            show(io, arrayref(e.args,i))
            print(io, ", ")
        end
        print(io, ")\n")
    end
end

## Load essential files and libraries

include("base.jl")

# core operations & types
include("range.jl")
include("tuple.jl")
include("cell.jl")
include("expr.jl")
include("error.jl")

# core numeric operations & types
include("bool.jl")
include("number.jl")
include("int.jl")
include("promotion.jl")
include("operators.jl")
include("pointer.jl")

_jl_lib = ccall(:jl_load_dynamic_library,Ptr{Void},(Ptr{None},),C_NULL)
_jl_libfdm = dlopen("libfdm")

include("float.jl")
include("reduce.jl")
include("complex.jl")
include("rational.jl")

# core data structures (used by type inference)
include("abstractarray.jl")
include("subarray.jl")
include("array.jl")
include("intset.jl")
include("dict.jl")
include("set.jl")

# compiler
include("inference.jl")

# I/O, strings & printing
include("io.jl")
include("char.jl")
include("ascii.jl")
include("utf8.jl")
include("string.jl")
include("regex.jl")
include("show.jl")
include("grisu.jl")
include("printf.jl")

# concurrency and parallelism
include("iterator.jl")
include("task.jl")
include("process.jl")
include("serialize.jl")
include("multi.jl")

# system & environment
include("osutils.jl")
include("libc.jl")
include("env.jl")
include("errno_h.jl")
include("file.jl")
include("stat.jl")

# front end
include("client.jl")

# core math functions
include("intfuncs.jl")
include("floatfuncs.jl")
include("math.jl")
include("math_libm.jl")
include("sort.jl")
include("combinatorics.jl")
include("statistics.jl")

# random number generation
include("random.jl")

# distributed arrays and memory-mapped arrays
include("darray.jl")
include("mmap.jl")

# utilities - version, timing, help, edit
include("version.jl")
include("util.jl")
include("datafmt.jl")
include("deepcopy.jl")

## Load optional external libraries

include("build_h.jl")

# linear algebra
include("linalg.jl")
include("linalg_dense.jl")
include("linalg_specialized.jl")
include("linalg_blas.jl")
include("linalg_lapack.jl")
include("factorizations.jl")

# signal processing
include("DSP_fftw.jl")
include("DSP.jl")
import Base.DSP.*

# prime method cache with some things we know we'll need right after startup
compile_hint(cwd, ())
compile_hint(fdio, (Int32,))
compile_hint(ProcessGroup, (Int, Array{Any,1}, Array{Any,1}))
compile_hint(select_read, (FDSet, Float64))
compile_hint(next, (Dict{Any,Any}, Int))
compile_hint(start, (Dict{Any,Any},))
compile_hint(perform_work, ())
compile_hint(isempty, (Array{Any,1},))
compile_hint(isempty, (Array{WorkItem,1},))
compile_hint(ref, (Dict{Any,Any}, Int32))
compile_hint(event_loop, (Bool,))
compile_hint(_start, ())
compile_hint(process_options, (Array{Any,1},))
compile_hint(run_repl, ())
compile_hint(anyp, (Function, Array{Any,1}))
compile_hint(Dict{Any,Any}, (Int,))
compile_hint(Set, ())
compile_hint(assign, (Dict{Any,Any}, Bool, Cmd))
compile_hint(rehash, (Dict{Any,Any}, Int))
compile_hint(run, (Cmd,))
compile_hint(spawn, (Cmd,))
compile_hint(assign, (Dict{Any,Any}, Bool, FileDes))
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
compile_hint(done, (IntSet, Int64))
compile_hint(next, (IntSet, Int64))
compile_hint(ht_keyindex, (Dict{Any,Any}, Int32))
compile_hint(perform_work, (WorkItem,))
compile_hint(notify_done, (WorkItem,))
compile_hint(work_result, (WorkItem,))
compile_hint(del_fd_handler, (Int32,))
compile_hint(enqueue, (Array{WorkItem,1}, WorkItem))
compile_hint(enq_work, (WorkItem,))
compile_hint(pop, (Array{WorkItem,1},))
compile_hint(string, (Int,))
compile_hint(parse_int, (Type{Int}, ASCIIString, Int))
compile_hint(repeat, (ASCIIString, Int))
compile_hint(KeyError, (Int,))
compile_hint(show, (Float64,))
compile_hint(match, (Regex, ASCIIString))
compile_hint(strlen, (ASCIIString,))
compile_hint(alignment, (Float64,))
compile_hint(repl_callback, (Expr, Int))
compile_hint(istaskdone, (Task,))
compile_hint(make_stdout_stream, ())
compile_hint(make_stdin_stream, ())
compile_hint(make_stderr_stream, ())
compile_hint(int, (Uint64,))
compile_hint(copy, (Bool,))
compile_hint(bool, (Bool,))
compile_hint(bool, (RemoteRef,))
compile_hint(wait, (RemoteRef,))
compile_hint(hash, (RemoteRef,))
compile_hint(take, (RemoteRef,))
compile_hint(bitmix, (Int, Int))
compile_hint(bitmix, (Uint, Int))
compile_hint(bitmix, (Uint64, Int64))
compile_hint(hash, (Int,))
compile_hint(isequal, (Symbol, Symbol))
compile_hint(isequal, (Bool, Bool))
compile_hint(WaitFor, (Symbol, RemoteRef))
compile_hint(_jl_answer_color, ())
compile_hint(get, (EnvHash, ASCIIString, ASCIIString))
compile_hint(notify_empty, (WorkItem,))
compile_hint(rr2id, (RemoteRef,))
compile_hint(isequal, (RemoteRef, WeakRef))
compile_hint(isequal, (RemoteRef, RemoteRef))
compile_hint(_ieval, (Symbol,))
compile_hint(static_convert, (Nothing, Nothing))
compile_hint(assign, (Array{Any,1}, WeakRef, Int))
compile_hint(assign, (Dict{Any,Any}, WorkItem, (Int,Int)))
compile_hint(isequal, ((Int,Int),(Int,Int)))
compile_hint(RemoteRef, (Int, Int, Int))
compile_hint(_jl_eval_user_input, (Expr, Bool))
compile_hint(print, (Float64,))
compile_hint(a2t, (Array{Any,1},))
compile_hint(flush, (IOStream,))
compile_hint(ref, (Type{String}, ASCIIString, ASCIIString, ASCIIString))
compile_hint(int, (Int,))
compile_hint(uint, (Uint,))
compile_hint(_atexit, ())
compile_hint(read, (IOStream, Array{Uint32,1}))
compile_hint(hex, (Char, Int))
compile_hint(abs, (Char,))
compile_hint(abstract_eval, (LambdaStaticData, ObjectIdDict, StaticVarInfo))
compile_hint(length, (Range1{Int},))
compile_hint(start, (Range1{Int},))
compile_hint(done, (Range1{Int},Int))
compile_hint(next, (Range1{Int},Int))
compile_hint(IOStream, (ASCIIString, Array{Uint8,1}))
compile_hint(mk_tupleref, (SymbolNode, Int))
compile_hint(abstract_interpret, (Bool, ObjectIdDict, StaticVarInfo))
compile_hint(eval_annotate, (LambdaStaticData, ObjectIdDict, StaticVarInfo, ObjectIdDict, Array{Any,1}))
compile_hint(occurs_more, (Bool, Function, Int))
compile_hint(isconstantfunc, (SymbolNode, StaticVarInfo))
compile_hint(CallStack, (Expr, Module, (Nothing,), EmptyCallStack))
compile_hint(convert, (Type{Module}, Module))
compile_hint(effect_free, (Expr,))
compile_hint(effect_free, (TopNode,))

# invoke type inference, running the existing inference code on the new
# inference code to cache an optimized version of it.
begin
    local atypes = (LambdaStaticData, Tuple, (), LambdaStaticData, Bool)
    local minf = methods(typeinf, atypes)
    typeinf_ext(minf[1][3], atypes, (), minf[1][3])
end

end # module Base

import Base.*

# create system image file
ccall(:jl_save_system_image, Void, (Ptr{Uint8},Ptr{Uint8}),
      "$JULIA_HOME/../lib/julia/sys.ji", "start_image.jl")
