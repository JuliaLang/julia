__precompile__()
module LibFoo

# Load in `deps.jl`, complaining if it does not exist
const depsjl_path = joinpath(dirname(@__FILE__), "..", "deps", "deps.jl")
if !isfile(depsjl_path)
    error("LibFoo not installed properly, run Pkg.build(\"LibFoo\"), restart Julia and try again")
end
include(depsjl_path)

# Module initialization function
function __init__()
    # Always check your dependencies from `deps.jl`
    check_deps()
end

# Export our two super-useful functions
export call_fooifier, call_libfoo


# Function to call the `fooifier` binary with the given arguments
function call_fooifier(a, b)
    global fooifier
    return parse(Float64, readchomp(`$fooifier $a $b`))
end

# Function to call into the `libfoo` shared library with the given arguments
function call_libfoo(a, b)
    global libfoo

    hdl = Libdl.dlopen_e(libfoo)
    @assert hdl != C_NULL "Could not open $libfoo"
    foo = Libdl.dlsym_e(hdl, :foo)
    @assert foo != C_NULL "Could not find foo() within $libfoo"
    return ccall(foo, Cdouble, (Cdouble, Cdouble), a, b)
end

end #module LibFoo