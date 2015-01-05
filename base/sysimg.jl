import Core.Intrinsics.ccall
ccall(:jl_new_main_module, Any, ())

baremodule Base

eval(x) = Core.eval(Base,x)
eval(m,x) = Core.eval(m,x)

include = Core.include

using Core: Intrinsics, arraylen, arrayref, arrayset, arraysize, _expr,
            tuplelen, tupleref, kwcall, _apply, typeassert, apply_type

include("exports.jl")

if false
    # simple print definitions for debugging. enable these if something
    # goes wrong during bootstrap before printing code is available.
    show(x::ANY) = ccall(:jl_static_show, Void, (Ptr{Void}, Any),
                         Intrinsics.pointerref(Intrinsics.cglobal(:jl_uv_stdout,Ptr{Void}),1), x)
    print(x::ANY) = show(x)
    println(x::ANY) = ccall(:jl_, Void, (Any,), x)
    print(a::ANY...) = for x=a; print(x); end
end


## Load essential files and libraries

include("base.jl")
include("reflection.jl")
include("build_h.jl")
include("version_git.jl")
include("c.jl")

# core operations & types
include("promotion.jl")
include("tuple.jl")
include("range.jl")
include("expr.jl")
include("error.jl")

# core numeric operations & types
include("bool.jl")
include("number.jl")
include("int.jl")
include("operators.jl")
include("pointer.jl")

# rounding utilities
include("rounding.jl")
importall .Rounding

include("float.jl")
include("complex.jl")
include("rational.jl")

# core data structures (used by type inference)
include("abstractarray.jl")
include("subarray.jl")
include("array.jl")
include("subarray2.jl")
include("bitarray.jl")
include("intset.jl")
include("dict.jl")
include("set.jl")
include("hashing.jl")
include("iterator.jl")

# SIMD loops
include("simdloop.jl")
importall .SimdLoop

include("reduce.jl")

# compiler
import Core.Undef  # used internally by compiler
include("inference.jl")

# For OS specific stuff in I/O
include("osutils.jl")

const DL_LOAD_PATH = ByteString[]
@osx_only push!(DL_LOAD_PATH, "@executable_path/../lib/julia")
@osx_only push!(DL_LOAD_PATH, "@executable_path/../lib")

# strings & printing
include("char.jl")
include("ascii.jl")
include("utf8.jl")
include("utf16.jl")
include("utf32.jl")
include("iobuffer.jl")
include("string.jl")
include("utf8proc.jl")
importall .UTF8proc
include("regex.jl")
include("base64.jl")
importall .Base64

# Core I/O
include("io.jl")
include("iostream.jl")

# system & environment
include("libc.jl")
include("env.jl")
include("errno.jl")
using .Errno
include("path.jl")
include("intfuncs.jl")


# I/O
include("task.jl")
include("show.jl")
include("stream.jl")
include("socket.jl")
include("stat.jl")
include("fs.jl")
importall .FS
include("process.jl")
include("multimedia.jl")
importall .Multimedia
ccall(:jl_get_uv_hooks, Void, ()) # TODO: should put this in _init
include("grisu.jl")
import .Grisu.print_shortest
include("file.jl")
include("methodshow.jl")

# core math functions
include("floatfuncs.jl")
include("math.jl")
importall .Math
const (√)=sqrt
const (∛)=cbrt
include("float16.jl")

# multidimensional arrays
include("cartesian.jl")
using .Cartesian
include("multidimensional.jl")

include("primes.jl")

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

# reduction along dims
include("reducedim.jl")  # macros in this file relies on string.jl

# basic data structures
include("ordering.jl")
importall .Order
include("collections.jl")

# Combinatorics
include("sort.jl")
importall .Sort
include("combinatorics.jl")

# version
include("version.jl")

# BigInts and BigFloats
include("gmp.jl")
importall .GMP
include("mpfr.jl")
importall .MPFR
big(n::Integer) = convert(BigInt,n)
big(x::FloatingPoint) = convert(BigFloat,x)
big(q::Rational) = big(num(q))//big(den(q))

# more hashing definitions
include("hashing2.jl")

# random number generation
include("dSFMT.jl")
include("random.jl")
importall .Random

# (s)printf macros
include("printf.jl")
importall .Printf

# nullable types
include("nullable.jl")

# concurrency and parallelism
include("serialize.jl")
include("multi.jl")

# code loading
include("loading.jl")

# Polling (requires multi.jl)
include("poll.jl")

# distributed arrays and memory-mapped arrays
include("darray.jl")
include("mmap.jl")
include("sharedarray.jl")

# utilities - timing, help, edit, metaprogramming
include("datafmt.jl")
importall .DataFmt
include("deepcopy.jl")
include("interactiveutil.jl")
include("replutil.jl")
include("test.jl")
include("meta.jl")
include("i18n.jl")
include("help.jl")
using .I18n
using .Help
push!(I18n.CALLBACKS, Help.clear_cache)

# frontend
include("Terminals.jl")
include("LineEdit.jl")
include("REPLCompletions.jl")
include("REPL.jl")
include("client.jl")

# Documentation

include("markdown/Markdown.jl")
include("docs.jl")
using .Docs
using .Markdown

# misc useful functions & macros
include("util.jl")

# sparse matrices and linear algebra
include("sparse.jl")
importall .SparseMatrix
include("linalg.jl")
importall .LinAlg
const ⋅ = dot
const × = cross
include("broadcast.jl")
importall .Broadcast

# statistics
include("statistics.jl")

# signal processing
include("fftw.jl")
include("dsp.jl")
importall .DSP

# system information
include("sysinfo.jl")
import .Sys.CPU_CORES

# mathematical constants
include("constants.jl")

# Numerical integration
include("quadgk.jl")
importall .QuadGK

# deprecated functions
include("deprecated.jl")

# package manager
include("pkg.jl")
const Git = Pkg.Git

# base graphics API
include("graphics.jl")

# profiler
include("profile.jl")
importall .Profile

# dates
include("Dates.jl")
import .Dates: Date, DateTime, now

# Some basic documentation
include("basedocs.jl")

function __init__()
    # Base library init
    reinit_stdio()
    Multimedia.reinit_displays() # since Multimedia.displays uses STDOUT as fallback
    fdwatcher_init()
    early_init()
    init_load_path()
end

include("precompile.jl")

include = include_from_node1

end # baremodule Base

using Base
importall Base.Operators

Base.isfile("userimg.jl") && Base.include("userimg.jl")
