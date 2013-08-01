baremodule Base

eval(x) = Core.eval(Base,x)
eval(m,x) = Core.eval(m,x)

include = Core.include

include("exports.jl")

## Load essential files and libraries

include("base.jl")
include("reflection.jl")
include("promotion.jl") # We need promote_type() before we can use composite types
include("build_h.jl")
include("c.jl")

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

# system & environment
include("libc.jl")
include("env.jl")
include("errno.jl")
using .Errno
include("path.jl")

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
ccall(:jl_get_uv_hooks, Void, ())
include("grisu.jl")
import .Grisu.print_shortest
include("printf.jl")
importall .Printf
include("file.jl")

# core math functions
include("intfuncs.jl")
include("floatfuncs.jl")
include("math.jl")
importall .Math
include("primes.jl")

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
include("pkg2.jl")

# base graphics API
include("graphics.jl")

# profiler
include("profile.jl")
importall .Profile

include("precompile.jl")

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
ccall(:jl_dump_bitcode, Void, (Ptr{Uint8},), "$JULIA_HOME/../$JL_PRIVATE_LIBDIR/sys.bc")
end
