# This file is a part of Julia. License is MIT: https://julialang.org/license

module Printf
# the macro implementations here exactly mirrors the
# macros left in base/printf.jl, and uses the utility there

export @printf, @sprintf
using Base.Printf

"""
    @printf([io::IOStream], "%Fmt", args...)

Print `args` using C `printf` style format specification string, with some caveats:
`Inf` and `NaN` are printed consistently as `Inf` and `NaN` for flags `%a`, `%A`,
`%e`, `%E`, `%f`, `%F`, `%g`, and `%G`. Furthermore, if a floating point number is
equally close to the numeric values of two possible output strings, the output
string further away from zero is chosen.

Optionally, an [`IOStream`](@ref)
may be passed as the first argument to redirect output.

See also: [`@sprintf`](@ref)

# Examples
```jldoctest
julia> @printf("%f %F %f %F\\n", Inf, Inf, NaN, NaN)
Inf Inf NaN NaN\n

julia> @printf "%.0f %.1f %f\\n" 0.5 0.025 -0.0078125
0 0.0 -0.007812
```
"""
:(@printf)

"""
    @sprintf("%Fmt", args...)

Return `@printf` formatted output as string.

# Examples
```jldoctest
julia> s = @sprintf "this is a %s %15.1f" "test" 34.567;

julia> println(s)
this is a test            34.6
```
"""
:(@sprintf)

end # module
