baremodule Base

eval(x) = Core.eval(Base,x)
eval(m,x) = Core.eval(m,x)

include = Core.include

include("exports.jl")

if false
    # simple print definitions for debugging. enable these if something
    # goes wrong during bootstrap before printing code is available.
    show(x::ANY) = ccall(:jl_static_show, Void, (Any,), x)
    print(x::ANY) = show(x)
    println(x::ANY) = ccall(:jl_, Void, (Any,), x)
    print(a::ANY...) = for x=a; print(x); end
end


## Load essential files and libraries

include("base.jl")
include("reflection.jl")
include("build_h.jl")
include("c.jl")

# core operations & types
include("promotion.jl")
include("range.jl")
include("tuple.jl")
include("cell.jl")
include("expr.jl")
include("error.jl")

# core numeric operations & types
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
include("iterator.jl")

# compiler
import Core.Undef  # used internally by compiler
include("inference.jl")

# For OS sprcific stuff in I/O
include("osutils.jl")

const DL_LOAD_PATH = ByteString[]
@osx_only push!(DL_LOAD_PATH, "@executable_path/../lib/julia")
@osx_only push!(DL_LOAD_PATH, "@executable_path/../lib")

# strings & printing
include("char.jl")
include("ascii.jl")
include("utf8.jl")
include("iobuffer.jl")
include("string.jl")
include("regex.jl")
include("base64.jl")
importall .Base64

# system & environment
include("libc.jl")
include("env.jl")
include("errno.jl")
using .Errno
include("path.jl")
include("intfuncs.jl")


# I/O
include("task.jl")
include("io.jl")
include("show.jl")
include("stream.jl")
include("socket.jl")
include("stat.jl")
include("fs.jl")
importall .FS
include("process.jl")
include("multimedia.jl")
importall .Multimedia
reinit_stdio()
ccall(:jl_get_uv_hooks, Void, ())
include("grisu.jl")
import .Grisu.print_shortest
include("printf.jl")
importall .Printf
include("file.jl")

# core math functions
include("floatfuncs.jl")
include("math.jl")
importall .Math
include("primes.jl")
include("float16.jl")

# concurrency and parallelism
include("serialize.jl")
include("multi.jl")

# Polling (requires multi.jl)
include("poll.jl")

# front end & code loading
include("repl.jl")
include("client.jl")
include("loading.jl")

begin
    SOURCE_PATH = ""
    include = function(path)
        prev = SOURCE_PATH
        path = joinpath(dirname(prev),path)
        SOURCE_PATH = path
        Core.include(path)
        SOURCE_PATH = prev
    end
end

# random number generation and statistics
include("statistics.jl")
include("librandom.jl")
include("random.jl")
importall .Random

# basic data structures
include("ordering.jl")
importall .Order
include("collections.jl")

# Combinatorics
include("sort.jl")
importall .Sort
include("combinatorics.jl")

# distributed arrays and memory-mapped arrays
include("darray.jl")
include("mmap.jl")

# utilities - version, timing, help, edit, metaprogramming
include("sysinfo.jl")
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
importall .SparseMatrix
include("matrixmarket.jl")
include("linalg.jl")
importall .LinAlg
include("broadcast.jl")
importall .Broadcast

# signal processing
include("fftw.jl")
include("dsp.jl")
importall .DSP

# rounding utilities
include("rounding.jl")
importall .Rounding

# BigInts and BigFloats
include("gmp.jl")
importall .GMP
include("mpfr.jl")
importall .MPFR
big(n::Integer) = convert(BigInt,n)
big(x::FloatingPoint) = convert(BigFloat,x)
big(q::Rational) = big(num(q))//big(den(q))
big(z::Complex) = complex(big(real(z)),big(imag(z)))

# mathematical constants
include("constants.jl")

# Numerical integration
include("quadgk.jl")
importall .QuadGK

# deprecated functions
include("deprecated.jl")

# git utils & package manager
include("git.jl")
include("pkg.jl")

# base graphics API
include("graphics.jl")

# profiler
include("profile.jl")
importall .Profile

include = include_from_node1

# prime method cache with some things we know we'll need right after startup
precompile(pwd, ())
precompile(fdio, (Int32,))
precompile(ProcessGroup, (Int, Array{Any,1}, Array{Any,1}))
precompile(next, (Dict{Any,Any}, Int))
precompile(start, (Dict{Any,Any},))
precompile(perform_work, ())
precompile(isempty, (Array{Any,1},))
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
precompile(perform_work, (Task,))
precompile(notify_full, (RemoteValue,))
precompile(notify_empty, (RemoteValue,))
precompile(work_result, (RemoteValue,))
precompile(take, (RemoteValue,))
precompile(wait_full, (RemoteValue,))
precompile(enq_work, (Task,))
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
precompile(get, (EnvHash, ASCIIString, ASCIIString))
precompile(rr2id, (RemoteRef,))
precompile(isequal, (RemoteRef, WeakRef))
precompile(isequal, (RemoteRef, RemoteRef))
precompile(_ieval, (Symbol,))
precompile(static_convert, (Nothing, Nothing))
precompile(setindex!, (Array{Any,1}, WeakRef, Int))
precompile(isequal, ((Int,Int),(Int,Int)))
precompile(isequal, (Int,Int))
precompile(RemoteRef, (Int, Int, Int))
precompile(eval_user_input, (Expr, Bool))
precompile(print, (Float64,))
precompile(a2t, (Array{Any,1},))
precompile(flush, (IOStream,))
precompile(getindex, (Type{ByteString}, ASCIIString, ASCIIString))
precompile(bytestring, (ASCIIString,))
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
precompile(print_joined, (IOBuffer, Array{String,1}, ASCIIString))
precompile(beginswith, (ASCIIString, ASCIIString))
precompile(resolve_globals, (Symbol, Module, Module, Vector{Any}, Vector{Any}))
precompile(resolve_globals, (SymbolNode, Module, Module, Vector{Any}, Vector{Any}))
precompile(BitArray, (Int,))
precompile(getindex, (BitArray{1}, Int,))
precompile(setindex!, (BitArray{1}, Bool, Int,))
precompile(fill!, (BitArray{1}, Bool))
precompile(pop!, (Array{Any,1},))
precompile(unshift!, (Array{Any,1}, Task))
precompile(nnz, (BitArray{1},))
precompile(get_chunks_id, (Int,))
precompile(occurs_more, (Uint8, Function, Int))
precompile(abstract_eval_arg, (Uint8, ObjectIdDict, StaticVarInfo))
precompile(occurs_outside_tupleref, (Function, Symbol, StaticVarInfo, Int))
precompile(search, (ASCIIString, Regex, Int))
precompile(setindex!, (Vector{Any}, Uint8, Int))
precompile(setindex!, (Vector{Any}, Vector{Any}, Int))
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
precompile(put, (RemoteRef, Any))
precompile(getpid, ())
precompile(print, (IOStream, Int32))
precompile(show, (IOStream, Int32))
precompile(open, (ASCIIString, ASCIIString))
precompile(readline, (ASCIIString,))
precompile(endof, (Array{Any,1},))
precompile(sym_replace, (Uint8, Array{Any,1}, Array{Any,1}, Array{Any,1}, Array{Any,1}))
precompile(isslotempty, (Dict{Any,Any}, Int))
precompile(setindex!, (Array{Uint8,1}, Uint8, Int))
precompile(get, (Dict{Any,Any}, Symbol, ASCIIString))
precompile(*, (ASCIIString, ASCIIString, ASCIIString))
precompile(chop, (ASCIIString,))
precompile(ismatch, (Regex, ASCIIString))
precompile(!=, (Bool, Bool))
precompile(nextind, (ASCIIString, Int))
precompile(delete_var!, (Expr, Symbol))
precompile(close, (IOStream,))
precompile(haskey, (ObjectIdDict, Symbol))

# invoke type inference, running the existing inference code on the new
# inference code to cache an optimized version of it.
begin
    local atypes = (LambdaStaticData, Tuple, (), LambdaStaticData, Bool)
    local minf = _methods(typeinf, atypes, -1)
    typeinf_ext(minf[1][3].func.code, atypes, (), minf[1][3].func.code)
end

end # baremodule Base

using Base.get, Base.ENV

let JL_PRIVATE_LIBDIR = get(ENV, "JL_PRIVATE_LIBDIR", "lib/julia")
# create system image file
ccall(:jl_save_system_image, Void, (Ptr{Uint8},),
      "$JULIA_HOME/../$JL_PRIVATE_LIBDIR/sys.ji")
end
