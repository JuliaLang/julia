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
    write(io::IO, a::Array{Uint8,1}) =
        ccall(:ios_write, Uint, (Ptr{Void}, Ptr{Void}, Uint),
              io.ios, a, length(a))
    print(io::IO, s::Symbol) = ccall(:jl_print_symbol, Void, (Ptr{Void},Any,),
                                 io.ios, s)
    print(io::IO, s::ASCIIString) = (write(io, s.data);nothing)
    print(io::IO, x) = show(io, x)
    println(io::IO, x) = (print(io, x); print(io, "\n"))
    show(io::IO, x) = ccall(:jl_show_any, Void, (Any, Any,), io, x)
    show(io::IO, s::ASCIIString) = (write(io, s.data);nothing)
    show(io::IO, s::Symbol) = print(io, s)
    show(io::IO, b::Bool) = print(io, b ? "true" : "false")
    show(io::IO, n::Int64) = ccall(:jl_print_int64, Void, (Ptr{Void}, Int64,), io, n)
    show(io::IO, n::Integer)  = show(io, int64(n))
    print(io::IO, a...) = for x=a; print(io, x); end
    function show(io::IO, e::Expr)
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
include("reflection.jl")
include("build_h.jl")
include("c.jl")

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
importall .FS
include("process.jl")
ccall(:jl_get_uv_hooks, Void, ())
include("char.jl")
include("ascii.jl")
include("utf8.jl")
include("string.jl")
include("regex.jl")
include("show.jl")
include("grisu.jl")
import .Grisu.print_shortest
include("printf.jl")
importall .Printf

# concurrency and parallelism
include("iterator.jl")
include("task.jl")
include("serialize.jl")
include("multi.jl")

# system & environment
include("osutils.jl")
include("libc.jl")
include("env.jl")
include("errno.jl")
using .Errno
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
importall .Math

# random number generation and statistics
include("statistics.jl")
include("librandom.jl")
include("random.jl")
importall .Random

# Combinatorics
include("sort.jl")
importall .Sort
include("combinatorics.jl")

# distributed arrays and memory-mapped arrays
include("darray2.jl")
include("mmap.jl")

# utilities - version, timing, help, edit, metaprogramming
include("version.jl")
include("datafmt.jl")
include("deepcopy.jl")
include("util.jl")
include("test.jl")
include("meta.jl")
include("i18n.jl")
include("help.jl")
using .I18n
using .Help
push!(I18n.CALLBACKS, Help.clear_cache)

# sparse matrices and linear algebra
include("sparse.jl")
include("linalg.jl")
importall .LinAlg

# signal processing
include("fftw.jl")
include("dsp.jl")
importall .DSP

# BigInts and BigFloats
include("gmp.jl")
importall .GMP

# deprecated functions
include("deprecated.jl")

# git utils & package manager
include("git.jl")
include("pkg.jl")

include = include_from_node1

# prime method cache with some things we know we'll need right after startup
precompile(pwd, ())
precompile(fdio, (Int32,))
precompile(ProcessGroup, (Int, Array{Any,1}, Array{Any,1}))
precompile(next, (Dict{Any,Any}, Int))
precompile(start, (Dict{Any,Any},))
precompile(perform_work, ())
precompile(isempty, (Array{Any,1},))
precompile(isempty, (Array{WorkItem,1},))
precompile(getindex, (Dict{Any,Any}, Int32))
precompile(event_loop, (Bool,))
precompile(_start, ())
precompile(process_options, (Array{Any,1},))
precompile(run_repl, ())
precompile(any, (Function, Array{Any,1}))
precompile(Dict{Any,Any}, (Int,))
precompile(Set, ())
precompile(setindex!, (Dict{Any,Any}, Bool, Cmd))
precompile(rehash, (Dict{Any,Any}, Int))
precompile(wait, (Int32,))
precompile(systemerror, (Symbol, Bool))
precompile(SystemError, (ASCIIString,))
precompile(has, (EnvHash, ASCIIString))
precompile(parse_input_line, (ASCIIString,))
precompile(cmp, (Int32, Int32))
precompile(min, (Int32, Int32))
precompile(==, (ASCIIString, ASCIIString))
precompile(arg_gen, (ASCIIString,))
precompile(Random.librandom_init, ())
precompile(Random.srand, (ASCIIString, Int))
precompile(Random.srand, (Uint64,))
precompile(open, (ASCIIString, Bool, Bool, Bool, Bool))
precompile(done, (IntSet, Int64))
precompile(next, (IntSet, Int64))
precompile(ht_keyindex, (Dict{Any,Any}, Int32))
precompile(perform_work, (WorkItem,))
precompile(notify_done, (WorkItem,))
precompile(work_result, (WorkItem,))
precompile(unshift!, (Array{WorkItem,1}, WorkItem))
precompile(enq_work, (WorkItem,))
precompile(pop!, (Array{WorkItem,1},))
precompile(string, (Int,))
precompile(parseint, (Type{Int}, ASCIIString, Int))
precompile(repeat, (ASCIIString, Int))
precompile(KeyError, (Int,))
precompile(show, (Float64,))
precompile(match, (Regex, ASCIIString))
precompile(length, (ASCIIString,))
precompile(alignment, (Float64,))
precompile(repl_callback, (Expr, Int))
precompile(istaskdone, (Task,))
precompile(int, (Uint64,))
precompile(copy, (Bool,))
precompile(bool, (Bool,))
precompile(bool, (RemoteRef,))
precompile(wait, (RemoteRef,))
precompile(hash, (RemoteRef,))
precompile(take, (RemoteRef,))
precompile(bitmix, (Int, Int))
precompile(bitmix, (Uint, Int))
precompile(bitmix, (Uint64, Int64))
precompile(hash, (Int,))
precompile(isequal, (Symbol, Symbol))
precompile(isequal, (Bool, Bool))
precompile(WaitFor, (Symbol, RemoteRef))
precompile(get, (EnvHash, ASCIIString, ASCIIString))
precompile(notify_empty, (WorkItem,))
precompile(rr2id, (RemoteRef,))
precompile(isequal, (RemoteRef, WeakRef))
precompile(isequal, (RemoteRef, RemoteRef))
precompile(_ieval, (Symbol,))
precompile(static_convert, (Nothing, Nothing))
precompile(setindex!, (Array{Any,1}, WeakRef, Int))
precompile(setindex!, (Dict{Any,Any}, WorkItem, (Int,Int)))
precompile(isequal, ((Int,Int),(Int,Int)))
precompile(isequal, (Int,Int))
precompile(RemoteRef, (Int, Int, Int))
precompile(eval_user_input, (Expr, Bool))
precompile(print, (Float64,))
precompile(a2t, (Array{Any,1},))
precompile(flush, (IOStream,))
precompile(getindex, (Type{ByteString}, ASCIIString, ASCIIString, ASCIIString, ASCIIString, ASCIIString))
precompile(int, (Int,))
precompile(uint, (Uint,))
precompile(_atexit, ())
precompile(read, (IOStream, Array{Uint32,1}))
precompile(hex, (Char, Int))
precompile(abs, (Char,))
precompile(abstract_eval, (LambdaStaticData, ObjectIdDict, StaticVarInfo))
precompile(length, (Range1{Int},))
precompile(start, (Range1{Int},))
precompile(done, (Range1{Int},Int))
precompile(next, (Range1{Int},Int))
precompile(IOStream, (ASCIIString, Array{Uint8,1}))
precompile(mk_tupleref, (SymbolNode, Int))
precompile(abstract_interpret, (Bool, ObjectIdDict, StaticVarInfo))
precompile(eval_annotate, (LambdaStaticData, ObjectIdDict, StaticVarInfo, ObjectIdDict, Array{Any,1}))
precompile(occurs_more, (Bool, Function, Int))
precompile(isconstantfunc, (SymbolNode, StaticVarInfo))
precompile(CallStack, (Expr, Module, (Nothing,), EmptyCallStack))
precompile(convert, (Type{Module}, Module))
precompile(effect_free, (Expr,))
precompile(effect_free, (TopNode,))
precompile(abspath, (ASCIIString,))
precompile(isabspath, (ASCIIString,))
precompile(split, (ASCIIString,))
precompile(split, (ASCIIString, ASCIIString, Int, Bool))
precompile(split, (ASCIIString, Regex, Int, Bool))
precompile(print_joined, (IOStream, Array{String,1}, ASCIIString))
precompile(beginswith, (ASCIIString, ASCIIString))
precompile(resolve_globals, (Symbol, Module, Module, Vector{Any}, Vector{Any}))
precompile(resolve_globals, (SymbolNode, Module, Module, Vector{Any}, Vector{Any}))
precompile(BitArray, (Int,))
precompile(getindex, (BitArray{1}, Int,))
precompile(setindex!, (BitArray{1}, Bool, Int,))
precompile(fill!, (BitArray{1}, Bool))
precompile(nnz, (BitArray{1},))
precompile(get_chunks_id, (Int,))
precompile(occurs_more, (Uint8, Function, Int))
precompile(abstract_eval_arg, (Uint8, ObjectIdDict, StaticVarInfo))
precompile(occurs_outside_tupleref, (Function, Symbol, StaticVarInfo, Int))
precompile(search, (ASCIIString, Regex, Int))
precompile(setindex!, (Vector{Any}, Uint8, Int))
precompile(first, (Range1{Int},))
precompile(last, (Range1{Int},))
precompile(isempty, (ASCIIString,))
precompile(normpath, (ASCIIString,))
precompile(print, (ASCIIString,))
precompile(println, (TTY,))
precompile(print, (TTY,Char))
precompile(==, (Bool,Bool))
precompile(try_include, (ASCIIString,))
precompile(is_file_readable, (ASCIIString,))
precompile(include_from_node1, (ASCIIString,))
precompile(source_path, (Nothing,))
precompile(task_local_storage, ())
precompile(atexit, (Function,))
precompile(print, (TTY, ASCIIString))
precompile(close, (TTY,))
precompile(readBuffer, (TTY,Int))
precompile(put, (RemoteRef, Any))
precompile(getpid, ())
precompile(print, (IOStream, Int32))
precompile(show, (IOStream, Int32))
precompile(open, (ASCIIString, ASCIIString))
precompile(readline, (ASCIIString,))
precompile(endof, (Array{Any,1},))
precompile(sym_replace, (Uint8, Array{Any,1}, Array{Any,1}, Array{Any,1}, Array{Any,1}))

# invoke type inference, running the existing inference code on the new
# inference code to cache an optimized version of it.
begin
    local atypes = (LambdaStaticData, Tuple, (), LambdaStaticData, Bool)
    local minf = methods(typeinf, atypes)
    typeinf_ext(minf[1][3], atypes, (), minf[1][3])
end

end # baremodule Base

using Base.get, Base.ENV

let JL_PRIVATE_LIBDIR = get(ENV, "JL_PRIVATE_LIBDIR", "lib/julia")
# create system image file
ccall(:jl_save_system_image, Void, (Ptr{Uint8},),
      "$JULIA_HOME/../$JL_PRIVATE_LIBDIR/sys.ji")
end
