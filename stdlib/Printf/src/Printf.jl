# This file is a part of Julia. License is MIT: https://julialang.org/license

__precompile__(true)

module Printf
# the implementations here is copied from
# what is left in base/printf.jl, and uses the utility there

export @printf, @sprintf, @format_str
export format, format_expr, printf, sprintf

import Base.Printf: is_str_expr, fix_dec, DIGITS, print_fixed, decode_dec, decode_hex,
                   ini_hex, ini_HEX, print_exp_a, decode_0ct, decode_HEX, ini_dec, print_exp_e,
                   decode_oct, _limit

using Base.Printf: gen_f, gen_e, gen_a, gen_g, gen_c, gen_s, gen_p, gen_d
using Unicode:  lowercase, textwidth

# copied from Base.Printf - amended by additional features:
# 1. optional argument positions %(\d+[$&])
# 2. special term %\d+% standing for %\d+&s

### generate code from parser output ###

function gen(s::AbstractString)
    args = []
    perm = Int[]
    blk = Expr(:block, :(local neg, pt, len, exp, do_out, args))
    blkst = blk.args
    for x in parse(s)
        if isa(x, AbstractString)
            push!(blkst, :(write(out, $(length(x)==1 ? x[1] : x))))
        else
            c = lowercase(x[end])
            f = c == 'f' ? gen_f :
                c == 'e' ? gen_e :
                c == 'a' ? gen_a :
                c == 'g' ? gen_g :
                c == 'c' ? gen_c :
                c == 's' ? gen_s :
                c == 'p' ? gen_p :
                           gen_d
            arg, ex = f(x[2:end]...)
            push!(args, arg)    # variable name and type ':(var::type)'
            push!(perm, x[1])   # position number
            push!(blkst, ex)    # executable statement write to 'out'
        end
    end
    push!(blkst, :nothing) # end of statement list in blk
    return args, blk, perm
end

### printf format string parsing ###

function parse(s::AbstractString)
    # parse format string into strings and format tuples
    curpos = 0
    list = []
    i = j = start(s)
    j1 = 0 # invariant: j1 == prevind(s, j)
    while !done(s,j)
        c, k = next(s,j)
        if c == '%'
            i > j1 || push!(list, s[i:j1])
            flags, width, precision, conversion, k, pos = parse1(s,k)
            '\'' in flags && throw(ArgumentError("printf format flag ' not yet supported"))
            conversion == 'n'    && throw(ArgumentError("printf feature %n not supported"))
            pos != 0 && conversion == '%' && ( conversion = 's' )
            if conversion == '%'
                push!(list, "%")
            else
                curpos = pos == 0 ? curpos + 1 : pos
                push!(list, (curpos,flags,width,precision,conversion))
            end
            i = k
        end
        j1 = j
        j = k
    end
    i > endof(s) || push!(list, s[i:end])
    # coalesce adjacent strings
    i = 1
    while i < length(list)
        if isa(list[i], AbstractString)
            for outer j = i+1:length(list)
                if !isa(list[j], AbstractString)
                    j -= 1
                    break
                end
                list[i] *= list[j]
            end
            deleteat!(list, i+1:j)
        end
        i += 1
    end
    return list
end

## step forward or bail out at end
function next_or_die(s::AbstractString, k)
    if !done(s,k)
        next(s,k)
    else
        throw(ArgumentError("invalid printf format string: $(repr(s))"))
    end
end

## parse a single printf specifier ##
# printf specifiers:
#   %                       # start
#   (\d+\[$&])?             # arg position
#   [\-\+#0' ]*             # flags
#   (\d+)?                  # width
#   (\.\d*)?                # precision
#   (h|hh|l|ll|L|j|t|z|q)?  # modifier (ignored)
#   [diouxXeEfFgGaAcCsSp%]  # conversion
#
# returns tuple (flags, width, precision, c, k, pos)
function parse1(s::AbstractString, k::Integer)
    j = k
    width = 0
    precision = -1
    pos = 0
    c, k = next_or_die(s,k)
    # handle %%
    if c == '%'
        return "", width, precision, c, k, pos
    end
    # look for optional argument position
    ca, ka = c, k
    # if we didn't allow 0 as first char of position no traceback rqd.
    while '0' <= ca <= '9'
        pos = 10*pos + ca-'0'
        ca, ka = next_or_die(s,ka)
    end
    if ka != k
        if ca == '$' || ca == '&' # ending position number - continue format
            j = ka
            c, k = next_or_die(s,ka)
        elseif ca == '%'          # ending position and default format
            return "s", width, precision, ca, ka, pos
        else
            pos = 0 # backtrace
        end
    end

    # parse flags
    while c in "#0- + '"
        c, k = next_or_die(s,k)
    end
    flags = String(s[j:prevind(s,k)-1]) # exploiting that all flags are one-byte.
    # parse width
    while '0' <= c <= '9'
        width = 10*width + c-'0'
        c, k = next_or_die(s,k)
    end
    # parse precision
    if c == '.'
        c, k = next_or_die(s,k)
        if '0' <= c <= '9'
            precision = 0
            while '0' <= c <= '9'
                precision = 10*precision + c-'0'
                c, k = next_or_die(s,k)
            end
        end
    end
    # parse length modifer (ignored)
    if c == 'h' || c == 'l'
        prev = c
        c, k = next_or_die(s,k)
        if c == prev
            c, k = next_or_die(s,k)
        end
    elseif c in "Ljqtz"
        c, k = next_or_die(s,k)
    end
    # validate conversion
    if !(c in "diouxXDOUeEfFgGaAcCsSp")
        throw(ArgumentError("invalid printf format string: $(repr(s))"))
    end
    # warn about silly flag/conversion combinations
    warn_unused(flags, width, precision, c)
    flags, width, precision, c, k, pos
end

function warn_unused(flags::String, width::Int, prec::Int, c::Char)
    c = lowercase(c)
    unc = c in "diucp" && '#' in flags
    unf = c in "csp" && flags != "" &&
        !( '-' in flags  || (c == 'c' && '0' in flags ) || (c == 's' && '#' in flags))
    unp = c in "cp" && prec >= 0

    if unc || unf || unp
        wt = width > 0 ? string(width) : ""
        pt = prec >= 0 ? string('.', prec) : ""
        tc = unf ? "flags " : ""
        tp = unp ? "precision " : ""
        text = string("unused ", tc, tp, "in format spec %", flags, wt, pt, c)
        throw(ArgumentError(text)) # or @warn text
    end
end

# generate code for macro expansion
# as format allow plain string literals (no interpolation) and raw string literals
#
function printf_expr(macroname, line, io, fmt, args)
    if isa(fmt, Expr) && fmt.head == :macrocall && fmt.args[1] == Symbol("@raw_str")
        fmt = fmt.args[end]
    end
    if !isa(fmt, AbstractString)
        throw(ArgumentError("$macroname: format must be a plain static string (no interpolation or macro prefix)"))
    end

    sargs, blk, perm = gen(fmt)

    bind_vars!(macroname, true, io, sargs, blk, perm, args)
end

## check relationship between arguments in format and provided data
# 1. for each format specifier ther must by exactly one data value
# 2. each data item has to be output by the format
# 3. the same data item may be re-used for several format specifiers
#
function check_args(mana, argl, argmax, perm)
    if argl != argmax
        throw(ArgumentError("$mana: wrong number of arguments ($argl) should be ($argmax)"))
    end
    if !( 1:argl ⊆ perm )
        throw(ArgumentError("$mana: invalid permutation '$perm'"))
    end
end

## Add code to pair symbolic args with data items sarg[i] := arg[perm[i]] by assignment
# macroname: identification for error messages
# ism: is called from macro
# io: output IO
# sargs: contain variable names and data types as provided by gen-subroutines
# blk: generated code
# perm: list of argument-indices associated to the variable names
# args: arguments from macro call or (already evaluated) from format-call
#
# note: The statements are added in reverse order of later execution
#       art the beginning of the given block.
#
function bind_vars!(macroname, ism::Bool, io, sargs, blk, perm, args)

    has_splatting = ism && any(arg -> isa(arg, Expr) && arg.head == :..., args)
    argmax = max(0, perm...)
    #
    #  Immediately check for corresponding arguments if there is no splatting
    #
    blkst = blk.args # the executable statements of the block
    if !has_splatting
        argl = length(args)
        check_args(macroname, argl, argmax, perm)
    end

    for i = length(sargs):-1:1
        var = sargs[i].args[1]
        if has_splatting
            # add assignment statement with values stored in runtime variable G
            pushfirst!(blkst, :($var = G[$(perm[i])]))
        else
            rhs = args[perm[i]]
            rhs = ism ? esc(rhs) : rhs
            if rhs != var
                # add assignment statement with constant values
                pushfirst!(blkst, :($var = $rhs))
            end
        end
    end

    #
    #  Delay generation of argument list and check until evaluation time instead of macro
    #  expansion time if there is splatting.
    #
    if has_splatting
       x = Expr(:call, :tuple, args...) # all arguments expanded into one tuple
       pushfirst!(blkst,
          quote
             G = $(esc(x))  # evaluate at runtime
             check_args($macroname, length(G), $argmax, $perm)
          end
       )
    end

    io != :out && pushfirst!(blkst, :(out = $io))
    Expr(:let, Expr(:block), blk)
end


"""
    @printf([io::IO,] "%Fmt", args...)

Print `args` using C `printf` style format specification string, with some caveats:
`Inf` and `NaN` are printed consistently as `Inf` and `NaN` for flags `%a`, `%A`,
`%e`, `%E`, `%f`, `%F`, `%g`, and `%G`. Furthermore, if a floating point number is
equally close to the numeric values of two possible output strings, the output
string further away from zero is chosen.
The variable length-specifiers `*` are not supported.
The argument-numbering syntax of C and C++ boost is supported, where the number delimiter
`\$` may be replaced by `&`.

Optionally, an `IO` stream of buffer
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
        printf_expr("@printf", __source__, STDOUT, args[1], args[2:end])
    else
        (length(args) >= 2 && (isa(args[2], AbstractString) || is_str_expr(args[2]))) ||
            throw(ArgumentError("@printf: first or second argument must be a format string"))
        printf_expr("@printf", __source__, esc(args[1]), args[2], args[3:end])
    end
end

"""
    @sprintf("%Fmt", args...)

Return `@printf` formatted output as string.

# Examples
```jldoctest
julia> s = @sprintf "this is a %2% s %1&15.1f" 34.567 "test";

julia> println(s)
this is a test            34.6
```
"""
macro sprintf(args...)
    isempty(args) && throw(ArgumentError("@sprintf: called with zero arguments"))
    isa(args[1], AbstractString) || is_str_expr(args[1]) ||
        throw(ArgumentError("@sprintf: first argument must be a format string"))
    letexpr = printf_expr("@sprintf", __source__, :(IOBuffer()), args[1], args[2:end])
    push!(letexpr.args[2].args, :(String(take!(out))))
    letexpr
end
"""
    format" literal format string"

Generate a format function f, which can be called like f(io, args...).
That has the same effect as `@printf(io, "format string", args..)` while the generated
code is not inlined, but in a separate local function.
The format function is generated only once for each different format string.
"""
macro format_str(arg)
    format_expr(__source__, arg)
end

##### formatting functions
## kind of type intersection, base on the symbols :Any, :Real, :Integer, :Ptr
# note: gen_d returns :Real (not :Integer)
#       gen_c returns :Integer (not :Char)
#
function typeinter(a::Symbol, b::Symbol)
    a === :Any ? b :
    b === :Any ? a :
    a === b ?    a :
    a === :Real ? b :
    b === :Real ? a :
                :PIC
end

## map to the actual argument types according to format specs
#
function typearg(a::Symbol)
    a === :Any ? a :
    a === :Real ? a :
    a === :Integer ? Expr(:curly, :Union, :Char, a) :
    a === :Ptr ? Expr(:curly, :Union, :Integer, a) :
                :Integer
end

## for all arguments determine the formal argument types
# to be accepted by the function
# Re-use the varaible names of the symbolic arguments
# If multiple format specs are delivered by the same argument
# the argument type must be restricted reasonably
#
function typeconsolidate(sargs, perm)
    n = max(0, perm...)
    m = length(perm)
    fargs = Vector{Any}(nothing, n)

    for i = 1:m
        j = perm[i]
        faj = fargs[j]
        if faj === nothing
            fargs[j] = sargs[i] # :(var::type)
        else
            ft = faj.args[2]
            st = sargs[i].args[2]
            if st != :Any
                if ft == :Any
                    faj.args[2] = st
                else
                    faj.args[2] = typeinter(ft, st)
                end
            end
        end
    end
    for j = 1:n
        faj = fargs[j]
        if faj === nothing
            fargs[j] = :(::Any) # unused argument without variable name
        else
            faj.args[2] = typearg(faj.args[2]) # final type
        end
    end
    fargs
end


## generate function definition for format function
#
function format_expr(line::LineNumberNode, fmt::AbstractString)
    sargs, blk, perm = gen(fmt)
    fargs = typeconsolidate(sargs, perm)
    args = map(p->p.args[1], fargs)
    blk = bind_vars!("format", false, :out, sargs, blk, perm, args)

    fun = :($(genfun())(out::IO) = nothing)
    append!(fun.args[1].args, fargs)
    fun.args[2].args[2] = blk
    fun.args[2].args[1] = line
    fun
end

# generate anonymous function
function format_fun(line::LineNumberNode, fmt)
    fun = eval(format_expr(line, fmt))
    f(io::IO, args...) = Base.invokelatest(fun, io, args...)
end
"""
    format(fmt::AbstractString) -> Function(::IO, args...)

Generate a format function f, which can be called like f(io, args...).
That has the same effect as `printf(io, "format string", args..)`.
The format function is generated only once for each different format string.

The variable length-specifiers `*` are not supported.
The argument-numbering syntax of C and C++ boost is supported, where the number delimiter
`\$` may be replaced by `&`.
The arguments are numberd starting with `1`. The arguments may be explicitly referred to
from the format string. In each format specification after the starting `%` and optional
argument number may be given in decimal digits, followed by `&` or `\$`. As an additional
feature copied from C++, `%<digits>%` behaves like `%<digits>&s`.

The number and type of the arguments is restricted by the format. All arguments need to
be used by the format. Each argument may be used multiple times.
The argument types are determined by the conversion character in the format:
```
    %c                              Char and Integer
    %d                              Real - printed value is rounded to Integer
    %a, %A, %e, %f, %F, %g, %G      Real
    %s                              Any
    %p                              Ptr and Integer
```
"""
function format(fmt::AbstractString)
    line = LineNumberNode(@__LINE__, Symbol(joinpath(@__DIR__, @__FILE__)))
    global ALL_FORMATS
    get!(ALL_FORMATS, fmt) do
        f = format_fun(line, fmt)
    end
end
"""
    printf([io::IO,] fmt, args...)
    sprintf(fmt, args) -> String

Print `args` using C `printf` style format specification string, with some caveats:
`Inf` and `NaN` are printed consistently as `Inf` and `NaN` for flags `%d`, `%s`, `%a`, `%A`,
`%e`, `%E`, `%f`, `%F`, `%g`, and `%G`. Furthermore, if a floating point number is
equally close to the numeric values of two possible output strings, the output
string further away from zero is chosen.
For further details of the format see [`format`](@ref).
The format `fmt` is not restricted to literal strings without interpolation syntax as is
the case for the corresponding macro calls.

Optionally, an `IO` stream or buffer may be passed as the first argument to redirect output.

# Examples
```jldoctest
julia> printf(STDERR, "%f %F %f %F\\n", Inf, Inf, NaN, NaN)
Inf Inf NaN NaN\n

julia> printf("%2&.0f %1\\\$.1f %3&f\\n", 0.025, 0.5, -0.0078125)
1 0.0 -0.007813
```
"""
printf(io::IO, fmt::Function, args...) = fmt(io, args...)
printf(fmt::Function, args...) = printf(STDOUT, fmt, args...)

"""
    sprintf(fmt, args...)

Return [`printf`](@ref) formatted output as string. See also [`format`](@ref).

# Examples
```jldoctest
julia> fmt = "this is a %2% s %1&15.1f"
       s = sprintf(fmt, 34.567, "test")

"this is a test s            34.6"

julia> println(s)
this is a test s            34.6
```
"""
function sprintf(fmt::Function, args...)
    io = IOBuffer()
    printf(io, fmt, args...)
    String(take!(io))
end

printf(io::IO, fmt::AbstractString, args...) = printf(io, format(fmt), args...)
printf(fmt::AbstractString, args...) = printf(format(fmt), args...)
sprintf(fmt::AbstractString, args...) = sprintf(format(fmt), args...)

# generate a syntactic correct unique function name in this Module
genfun() = Symbol("fmt", replace(string(gensym()), '#'=>'_'))

"""
    ALL_FORMATS::Dict{String,Function}

Global variable storing all generated format functions generated during the first
invocation of `format`, `printf`, `sprintf`, or `@format`. Not for `@printf` and
`@sprintf`.
"""
const ALL_FORMATS = Dict{String, Function}()


end # module
