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
include("options.jl")

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
include("refpointer.jl")

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
include("functors.jl")
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
include("inference.jl")

# For OS specific stuff in I/O
include("osutils.jl")

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
using .Libc: getpid, gethostname, time, msync
include("libdl.jl")
include("env.jl")
include("path.jl")
include("intfuncs.jl")

# nullable types
include("nullable.jl")

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

const _fact_table128 =
    UInt128[0x00000000000000000000000000000001, 0x00000000000000000000000000000002,
            0x00000000000000000000000000000006, 0x00000000000000000000000000000018,
            0x00000000000000000000000000000078, 0x000000000000000000000000000002d0,
            0x000000000000000000000000000013b0, 0x00000000000000000000000000009d80,
            0x00000000000000000000000000058980, 0x00000000000000000000000000375f00,
            0x00000000000000000000000002611500, 0x0000000000000000000000001c8cfc00,
            0x0000000000000000000000017328cc00, 0x0000000000000000000000144c3b2800,
            0x00000000000000000000013077775800, 0x00000000000000000000130777758000,
            0x00000000000000000001437eeecd8000, 0x00000000000000000016beecca730000,
            0x000000000000000001b02b9306890000, 0x000000000000000021c3677c82b40000,
            0x0000000000000002c5077d36b8c40000, 0x000000000000003ceea4c2b3e0d80000,
            0x000000000000057970cd7e2933680000, 0x00000000000083629343d3dcd1c00000,
            0x00000000000cd4a0619fb0907bc00000, 0x00000000014d9849ea37eeac91800000,
            0x00000000232f0fcbb3e62c3358800000, 0x00000003d925ba47ad2cd59dae000000,
            0x0000006f99461a1e9e1432dcb6000000, 0x00000d13f6370f96865df5dd54000000,
            0x0001956ad0aae33a4560c5cd2c000000, 0x0032ad5a155c6748ac18b9a580000000,
            0x0688589cc0e9505e2f2fee5580000000, 0xde1bc4d19efcac82445da75b00000000]
factorial(n::Int128) = factorial_lookup(n, _fact_table128, 33)
factorial(n::UInt128) = factorial_lookup(n, _fact_table128, 34)

# more hashing definitions
include("hashing2.jl")

# random number generation
include("dSFMT.jl")
include("random.jl")
importall .Random

# (s)printf macros
include("printf.jl")
importall .Printf

# concurrency and parallelism
include("serialize.jl")
include("multi.jl")
include("managers.jl")

# code loading
include("loading.jl")

# Polling (requires multi.jl)
include("poll.jl")

# memory-mapped and shared arrays
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

# dense linear algebra
include("linalg.jl")
importall .LinAlg
const ⋅ = dot
const × = cross
include("broadcast.jl")
importall .Broadcast

# sparse matrices and sparse linear algebra
include("sparse.jl")
importall .SparseMatrix

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

# Fast math
include("fastmath.jl")
importall .FastMath

# package manager
include("pkg.jl")
const Git = Pkg.Git

# profiler
include("profile.jl")
importall .Profile

# dates
include("Dates.jl")
import .Dates: Date, DateTime, now

# enums
include("Enums.jl")
importall .Enums

# deprecated functions
include("deprecated.jl")

# Some basic documentation
include("basedocs.jl")

function __init__()
    # Base library init
    reinit_stdio()
    Multimedia.reinit_displays() # since Multimedia.displays uses STDOUT as fallback
    fdwatcher_init()
    early_init()
    init_load_path()
    init_parallel()
end

include("precompile.jl")

include = include_from_node1

end # baremodule Base

using Base
importall Base.Operators

Base.isfile("userimg.jl") && Base.include("userimg.jl")
