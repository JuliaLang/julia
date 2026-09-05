# This file is a part of Julia. License is MIT: https://julialang.org/license

# Read a profile written by a precompilation worker, as enabled by
# `precompilepkgs(profile=...)` or the `JULIA_PRECOMPILE_PROFILE` environment
# variable, and hand it to `Profile` for analysis.
#
#     include("contrib/read_precompile_profile.jl")
#     data, lidict = read_precompile_profile("/tmp/jl_precompile_profile_XXXX/Makie-12345")
#     Profile.print(; C=true)  # see the docstring below for the exact call
#
# The worker writes two files because instruction pointers can only be resolved
# in the process that recorded them: `<prefix>.profdata` holds the raw sample
# buffer and `<prefix>.profsyms` the stack frames those pointers resolve to.

using Profile
using Base.StackTraces: StackFrame

"""
    read_precompile_profile(prefix) -> (data, lidict)

Load the precompile-worker profile written to `\$prefix.profdata` and
`\$prefix.profsyms`. `prefix` may also be either of those paths.

The result is accepted directly by the `Profile` reporting functions:

```julia
data, lidict = read_precompile_profile(prefix)
Profile.print(stdout, data, lidict; C=true, format=:tree)
```
"""
function read_precompile_profile(prefix::AbstractString)
    prefix = replace(String(prefix), r"\.(profdata|profsyms)$" => "")
    data = collect(reinterpret(UInt, read(prefix * ".profdata")))
    lidict = Dict{UInt64,Vector{StackFrame}}()
    for line in eachline(prefix * ".profsyms")
        (isempty(line) || startswith(line, '#')) && continue
        ip_str, inlined, lineno, from_c, func, file = split(line, '\t'; limit=6)
        ip = parse(UInt64, ip_str)
        frame = StackFrame(Symbol(func), Symbol(file), parse(Int, lineno), nothing,
                           from_c == "1", inlined == "1", ip)
        push!(get!(Vector{StackFrame}, lidict, ip), frame)
    end
    return data, lidict
end

"""
    print_precompile_profile(prefix; kwargs...)

Read the profile at `prefix` and print it, passing `kwargs` on to `Profile.print`.
"""
function print_precompile_profile(prefix::AbstractString; io::IO=stdout, kwargs...)
    data, lidict = read_precompile_profile(prefix)
    Profile.print(io, data, lidict; kwargs...)
end
