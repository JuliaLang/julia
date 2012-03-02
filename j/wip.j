module System

if true
    # simple print definitions for debugging. enable these if something
    # goes wrong during bootstrap before printing code is available.
    length(a::Array) = arraylen(a)
    print(a::Array{Uint8,1}) = ccall(:jl_print_array_uint8, Void, (Any,), a)
    print(s::Symbol) = ccall(:jl_print_symbol, Void, (Any,), s)
    print(s::ASCIIString) = print(s.data)
    print(x) = show(x)
    println(x) = (print(x);print("\n"))
    show(x) = ccall(:jl_show_any, Void, (Any,), x)
    show(s::ASCIIString) = print(s.data)
    show(s::Symbol) = print(s)
    show(b::Bool) = print(b ? "true" : "false")
    show(n::Int64) = ccall(:jl_print_int64, Void, (Int64,), n)
    show(n::Integer)  = show(int64(n))
    print(a...) = for x=a; print(x); end
    function show(e::Expr)
        print(e.head)
        print("(")
        for i=1:arraylen(e.args)
            show(arrayref(e.args,i))
            print(", ")
        end
        print(")\n")
    end
end
## Load essential files and libraries

include("base.j")

# core operations & types
include("range.j")
include("tuple.j")
include("cell.j")
include("expr.j")
include("error.j")

# core numeric operations & types
include("bool.j")
include("number.j")
include("int.j")
include("promotion.j")
include("operators.j")
include("float.j")
include("pointer.j")
include("char.j")
include("reduce.j")
include("complex.j")
include("rational.j")

# core data structures (used by type inference)
include("abstractarray.j")
include("subarray.j")
include("array.j")
include("intset.j")
include("table.j")
include("set.j")

# compiler
include("inference.j")

# I/O, strings & printing
include("io.j")
include("string.j")
include("ascii.j")
include("utf8.j")
include("regex.j")
include("show.j")
include("grisu.j")
include("printf.j")

# system & environment
include("libc.j")
include("env.j")
include("errno_h.j")

# core math functions
include("intfuncs.j")
include("floatfuncs.j")
include("math.j")
include("math_libm.j")
include("sort.j")
include("combinatorics.j")
include("statistics.j")

# concurrency and parallelism
include("iterator.j")
include("task.j")
include("stream.j")

set_current_output_stream(make_stdout_stream()) # for error reporting
end #module
