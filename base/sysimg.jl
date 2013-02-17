baremodule Base

eval(x) = Core.eval(Base,x)
eval(m,x) = Core.eval(m,x)

include = Core.include

include("exports.jl")

if false
    # simple print definitions for debugging. enable these if something
    # goes wrong during bootstrap before printing code is available.
    length(a::Array) = arraylen(a)
    print(x) = print(STDOUT, x)
    show(x) = show(STDOUT, x)
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
include("promotion.jl")
include("bool.jl")
include("number.jl")
include("int.jl")
include("operators.jl")
include("pointer.jl")

include("float.jl")
include("reduce.jl")
include("complex.jl")
include("rational.jl")

# core data structures (used by type inference)
include("abstractarray.jl")
include("subarray.jl")
include("array.jl")
include("bitarray.jl")
include("intset.jl")
include("dict.jl")
include("set.jl")

# compiler
import Core.Undef  # used internally by compiler
include("inference.jl")

# I/O, strings & printing
include("io.jl")
include("iobuffer.jl")
include("stream.jl")
include("socket.jl")
include("fs.jl")
importall FS
include("process.jl")
ccall(:jl_get_uv_hooks, Void, ())
include("char.jl")
include("ascii.jl")
include("utf8.jl")
include("string.jl")
include("regex.jl")
include("show.jl")
include("grisu.jl")
import Grisu.print_shortest
include("printf.jl")
importall Printf

# concurrency and parallelism
include("iterator.jl")
include("task.jl")
include("serialize.jl")
include("multi.jl")

# system & environment
include("build_h.jl")
include("osutils.jl")
include("libc.jl")
include("env.jl")
include("errno.jl")
using Errno
include("file.jl")
include("path.jl")
include("stat.jl")

# front end & code loading
include("repl.jl")
include("client.jl")
include("loading.jl")

# core math functions
include("intfuncs.jl")
include("floatfuncs.jl")
include("math.jl")
importall Math

# random number generation and statistics
include("statistics.jl")
include("librandom.jl")
include("random.jl")
importall Random

# Combinatorics
include("sort.jl")
importall Sort
include("combinatorics.jl")

# distributed arrays and memory-mapped arrays
#include("darray.jl")
include("darray2.jl")
include("mmap.jl")

# utilities - version, timing, help, edit, metaprogramming
include("version.jl")
include("datafmt.jl")
include("deepcopy.jl")
include("util.jl")
include("test.jl")
include("meta.jl")

# linear algebra
include("blas.jl")
include("lapack.jl")
include("matmul.jl")
include("sparse.jl")
include("linalg.jl")
include("linalg_dense.jl")
include("linalg_bitarray.jl")
include("linalg_sparse.jl")

# signal processing
include("fftw.jl")
include("dsp.jl")
importall DSP

# BigInts and BigFloats
include("gmp.jl")
importall GMP

# deprecated functions
include("deprecated.jl")

# git utils & package manager
include("git.jl")
include("pkg.jl")

include = include_from_node1

# prime method cache with some things we know we'll need right after startup
compile_hint(pwd, ())
compile_hint(fdio, (Int32,))
compile_hint(ProcessGroup, (Int, Array{Any,1}, Array{Any,1}))
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
compile_hint(any, (Function, Array{Any,1}))
compile_hint(Dict{Any,Any}, (Int,))
compile_hint(Set, ())
compile_hint(assign, (Dict{Any,Any}, Bool, Cmd))
compile_hint(rehash, (Dict{Any,Any}, Int))
#compile_hint(run, (Cmd,))
#compile_hint(spawn, (Cmd,))
#compile_hint(assign, (Dict{Any,Any}, Bool, FileDes))
compile_hint(wait, (Int32,))
compile_hint(system_error, (Symbol, Bool))
compile_hint(SystemError, (ASCIIString,))
compile_hint(has, (EnvHash, ASCIIString))
compile_hint(parse_input_line, (ASCIIString,))
compile_hint(cmp, (Int32, Int32))
compile_hint(min, (Int32, Int32))
compile_hint(==, (ASCIIString, ASCIIString))
compile_hint(arg_gen, (ASCIIString,))
compile_hint(Random.librandom_init, ())
compile_hint(Random.srand, (ASCIIString, Int))
compile_hint(Random.srand, (Uint64,))
compile_hint(open, (ASCIIString, Bool, Bool, Bool, Bool))
compile_hint(done, (IntSet, Int64))
compile_hint(next, (IntSet, Int64))
compile_hint(ht_keyindex, (Dict{Any,Any}, Int32))
compile_hint(perform_work, (WorkItem,))
compile_hint(notify_done, (WorkItem,))
compile_hint(work_result, (WorkItem,))
compile_hint(unshift!, (Array{WorkItem,1}, WorkItem))
compile_hint(enq_work, (WorkItem,))
compile_hint(pop!, (Array{WorkItem,1},))
compile_hint(string, (Int,))
compile_hint(parse_int, (Type{Int}, ASCIIString, Int))
compile_hint(repeat, (ASCIIString, Int))
compile_hint(KeyError, (Int,))
compile_hint(show, (Float64,))
compile_hint(match, (Regex, ASCIIString))
compile_hint(length, (ASCIIString,))
compile_hint(alignment, (Float64,))
compile_hint(repl_callback, (Expr, Int))
compile_hint(istaskdone, (Task,))
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
compile_hint(isequal, (Int,Int))
compile_hint(RemoteRef, (Int, Int, Int))
compile_hint(eval_user_input, (Expr, Bool))
compile_hint(print, (Float64,))
compile_hint(a2t, (Array{Any,1},))
compile_hint(flush, (IOStream,))
compile_hint(ref, (Type{ByteString}, ASCIIString, ASCIIString, ASCIIString, ASCIIString, ASCIIString, ASCIIString))
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
compile_hint(abspath, (ASCIIString,))
compile_hint(isabspath, (ASCIIString,))
compile_hint(split, (ASCIIString,))
compile_hint(split, (ASCIIString, ASCIIString, Int, Bool))
compile_hint(split, (ASCIIString, Regex, Int, Bool))
compile_hint(print_joined, (IOStream, Array{String,1}, ASCIIString))
compile_hint(begins_with, (ASCIIString, ASCIIString))
compile_hint(resolve_globals, (Symbol, Module, Module, Vector{Any}, Vector{Any}))
compile_hint(resolve_globals, (SymbolNode, Module, Module, Vector{Any}, Vector{Any}))
compile_hint(BitArray, (Int,))
compile_hint(ref, (BitArray{1}, Int,))
compile_hint(assign, (BitArray{1}, Bool, Int,))
compile_hint(fill!, (BitArray{1}, Bool))
compile_hint(nnz, (BitArray{1},))
compile_hint(get_chunks_id, (Int,))
compile_hint(occurs_more, (Uint8, Function, Int))
compile_hint(abstract_eval_arg, (Uint8, ObjectIdDict, StaticVarInfo))
compile_hint(occurs_outside_tupleref, (Function, Symbol, StaticVarInfo, Int))
compile_hint(search, (ASCIIString, Regex, Int))
compile_hint(assign, (Vector{Uint8}, Uint8, Int))

# invoke type inference, running the existing inference code on the new
# inference code to cache an optimized version of it.
begin
    local atypes = (LambdaStaticData, Tuple, (), LambdaStaticData, Bool)
    local minf = methods(typeinf, atypes)
    typeinf_ext(minf[1][3], atypes, (), minf[1][3])
end

end # baremodule Base

using Base

let JL_PRIVATE_LIBDIR = get(ENV, "JL_PRIVATE_LIBDIR", "lib/julia")
# create system image file
ccall(:jl_save_system_image, Void, (Ptr{Uint8},),
      "$JULIA_HOME/../$JL_PRIVATE_LIBDIR/sys.ji")
end
