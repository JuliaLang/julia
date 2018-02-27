# This file is a part of Julia. License is MIT: https://julialang.org/license

__precompile__(true)

module Printf
# the macro implementations here exactly mirrors the
# macros left in base/printf.jl, and uses the utility there

export @printf, @sprintf
using Base.Printf: _printf, is_str_expr, fix_dec, DIGITS, print_fixed, decode_dec, decode_hex,
                   ini_hex, ini_HEX, print_exp_a, decode_0ct, decode_HEX, ini_dec, print_exp_e,
                   decode_oct, _limit
using Unicode: textwidth

"""
    @printf([io::IOStream], "%Fmt", args...)

Print `args` using C `printf` style format specification string, with some caveats:
`Inf` and `NaN` are printed consistently as `Inf` and `NaN` for flags `%a`, `%A`,
`%e`, `%E`, `%f`, `%F`, `%g`, and `%G`. Furthermore, if a floating point number is
equally close to the numeric values of two possible output strings, the output
string further away from zero is chosen.

Optionally, an `IOStream`
may be passed as the first argument to redirect output.

# Examples
```jldoctest
julia> @printf("%f %F %f %F\\n", Inf, Inf, NaN, NaN)
Inf Inf NaN NaN\n

julia> @printf "%.0f %.1f %f\\n" 0.5 0.025 -0.0078125
1 0.0 -0.007813
```
"""
macro printf(args...)
    isempty(args) && throw(ArgumentError("@printf: called with no arguments"))
    if isa(args[1], AbstractString) || is_str_expr(args[1])
        _printf("@printf", :stdout, args[1], args[2:end])
    else
        (length(args) >= 2 && (isa(args[2], AbstractString) || is_str_expr(args[2]))) ||
            throw(ArgumentError("@printf: first or second argument must be a format string"))
        _printf("@printf", esc(args[1]), args[2], args[3:end])
    end
end

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
macro sprintf(args...)
    isempty(args) && throw(ArgumentError("@sprintf: called with zero arguments"))
    isa(args[1], AbstractString) || is_str_expr(args[1]) ||
        throw(ArgumentError("@sprintf: first argument must be a format string"))
    letexpr = _printf("@sprintf", :(IOBuffer()), args[1], args[2:end])
    push!(letexpr.args[2].args, :(String(take!(out))))
    letexpr
end

end # module
