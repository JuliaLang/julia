# This file is a part of Julia. License is MIT: https://julialang.org/license

# This file depends on base/regex.jl and base/show.jl
# and needs to follow these in base/sysimg.jl

"""
    Module to implement replace method with multiple pairs of patterns-replacements
"""
module ReplaceImpl

export replace_code
import Base: Callable, SubstitutionString

# types of input patterns
const TIF = Union{Char,AbstractString,Regex,Callable,Tuple{Vararg{Char}},AbstractVector{Char},Set{Char}}
# types of input replacements
#const TIS = Union{Char,AbstractString,Callable,SubstitutionString}
const TIS = Any

# types input patterns are converted to
const TF = Union{Regex,String,Callable}
# types output patterns are converted to
const TS = Union{Char,String,Callable,SubstitutionString}

# from reflection.jl::hasmethod
function type_hasmethod(@nospecialize(ft::DataType), @nospecialize(t), world=typemax(UInt))
    t = Tuple{ft, t...}
    ccall(:jl_method_exists, Cint, (Any, Any, UInt), ft.name.mt, t, world) != 0
end

# code generation for all input patterns
function gen_pattern_all(ptypes::NTuple{N,DataType}) where N
    Expr(:block, [gen_pattern(i, ptypes[i]) for i in 1:N]...)
end

# generate code to find next substring complying Regex or identical to String
function gen_pattern(pri::Int, ::Type{<:Pair{Regex}})
    :(  let j = $(getpr(1, pri)), k = $(getpr(2, pri)), r;
        if j < MAX
            if j < pos
                r = findnext($(getpa(pri)), str, pos)
                j, k = first(r), last(r)
                j = ifelse(j == 0, MAX, j)
                $(setpr(1, pri, :j)); $(setpr(2, pri, :k))
            end
            $(gen_min_max(pri))
        end
    end
    )
end

function gen_pattern(pri::Int, ::Type{<:Pair{String}})
    :(  let j = $(getpr(1, pri)), k = $(getpr(2, pri));
        if j < MAX
            if j < pos
                k = endof($(getpa(pri)))
                if k > 0
                    j = Base._searchindex(cstr, $(getpavec(pri)), pos)
                    if j == 0
                        j = k = MAX
                    else
                        k += j - 1
                    end
                else
                    j = pos; k = pos - 1
                end
                $(setpr(1, pri, :j)); $(setpr(2, pri, :k))
            end
            $(gen_min_max(pri))
        end
    end
    )
end

# generate code to find next character honoring predicate function
function gen_pattern(pri::Int, ::Type{<:Pair{<:Function}})
    :(  let j = $(getpr(1, pri)), k;
        if j < MAX && pos != maxk
            if j < pos
                r = findnext($(getpa(pri)), str, pos)
                j = r isa Nothing ? MAX : r
                $(setpr(1, pri, :j)); $(setpr(2, pri, :j))
            end
            k = j
            $(gen_min_max(pri))
        end
    end
    )
end

# generate code to calculate minj, maxk, and minp
function gen_min_max(pri::Int)
    :( (j < minj || j == minj && k > maxk) && ((minj, maxk, minp) = (j, k, $pri)) )
end

# generate code to select one of several options, depending on runtime variable `minp`.
function extree(n::Int, m::Int, ex::Vector)
    if n < m
        k = (n + m) ÷ 2
        #Expr(:if, :(minp <= $k), extree(n, k, ex), extree(k+1, m, ex))
        :( minp <= $k ? $(extree(n, k, ex)) : $(extree(k+1, m, ex)) )
    elseif n == m
        ex[n]
    else
        nothing
    end
end

# generate code to access variables
getpr(k::Int, pri::Int) = Symbol("pr", k, "_", pri)
setpr(k::Int, pri::Int, j) = Expr(:(=), getpr(k, pri), j)
setprall(k::Int, n::Int) = [setpr(k, i, 0) for i = 1:n]

getpa(pri::Int) = Symbol("pa_", pri)
setpa(pri::Int, T::Type, j) = Expr(:(=), Expr(:(::), getpa(pri), T), j)
setpaall(pt::NTuple{N,DataType}) where N = [setpa(i, pt[i].types[1], :(pat_repls[$i].first)) for i = 1:N]

getpavec(pri::Int) = Symbol("pavec_", pri)
setpavec(pri::Int, T::Type, j) = Expr(:(=), Expr(:(::), getpavec(pri), T), j)
function setpavecall(pt::NTuple{N,DataType}) where N
    ex = Expr[]
    for i = 1:N
        if pt[i].types[1] == String
            push!(ex, setpavec(i, Vector{UInt8}, :(unsafe_wrap(Vector{UInt8}, $(getpa(i))))))
        end
    end
    ex
end

getre(pri::Int) = Symbol("re_", pri)
setre(pri::Int, T::Type, j) = Expr(:(=), Expr(:(::), getre(pri), T), j)
setreall(pt::NTuple{N,DataType}) where N = [setre(i, pt[i].types[2], :(pat_repls[$i].second)) for i = 1:N]

function gen_repl_all(ptypes::NTuple{N,DataType}) where N
    extree(1, N, [gen_repl(i, ptypes[i]) for i in 1:N])
end

# code generation depending on pattern_type
function gen_repl(pri::Int, ::Type{<:Pair{<:Any, Char}})
    :( write(out, $(getre(pri))))
end

function gen_repl(pri::Int, ::Type{<:Pair{<:Any, String}})
    :( print(out, $(getre(pri))))
end

# call the output methods for (Char,)
function gen_repl(pri::Int, pt::Type{<:Pair{<:Callable, <:Callable}})
    if type_hasmethod(pt.parameters[2], (Char,))
        :( write(out, $(getre(pri))(getindex(str, minj))))
    else
        :( write(out, $(getre(pri))(getindex(str, minj:minj))))
    end
end

function gen_repl(pri::Int, pt::Type{<:Pair{<:Any, <:Callable}})
    if type_hasmethod(pt.parameters[2], (Char,))
        :( minj == maxk ?
            write(out, $(getre(pri))(getindex(str, minj))) :
            write(out, $(getre(pri))(getindex(str, minj:maxk))) )
    else
        :( write(out, $(getre(pri))(getindex(str, minj:maxk))))
    end
end

function gen_repl(pri::Int, ::Type{<:Pair{Regex, <:SubstitutionString}})
    :(Base._replace(out, $(getre(pri)), str, minj:maxk, $(getpa(pri))))
end

# all arguments are types of the real arguments
function replace_gen_impl(str::Type{String}, count::Type{Int}, pat_repls...)
    n = length(pat_repls)
    gen_input = gen_pattern_all(pat_repls)
    gen_output = gen_repl_all(pat_repls)

    quote
        begin
            # The following local variables occur once per pair
            $(setpaall(pat_repls)...)       # patterns
            $(setpavecall(pat_repls)...)    # UInt8-vector forms of string pattern
            $(setreall(pat_repls)...)       # replacements
            $(setprall(1, n)...)            # next start of match
            $(setprall(2, n)...)            # next end of match
            cstr = unsafe_wrap(Vector{UInt8}, str)  # vector form of input string
            MAX = typemax(Int)
            eos = endof(str)
            pos = 1  # start position for next findnext
            wpos = 1 # start of write out in original string
            # buffer size hint greater than original size - empirical value
            out = IOBuffer(Base.StringVector(eos*12÷10), true, true)
            out.size = 0
            ctr = count
            while ctr > 0

                minj = MAX  # first of next found pattern (MAX if nothing found)
                maxk = -1   # end of next found pattern
                minp = 0    # type of repl for found pattern
                $gen_input  # calculates minj, maxk, minp
                minj == MAX && break    # no further pattern found
                if wpos == 1 || wpos <= maxk
                    unsafe_write(out, pointer(str, wpos), UInt(minj-wpos))
                    $gen_output
                end
                if maxk < minj   # empty pattern found
                    wpos = minj  # write this position
                    minj > eos && break
                    pos = nextind(str, minj) # but skip searching next pattern
                else
                    pos = wpos = nextind(str, maxk) # write and search from after pattern
                end
                ctr -= 1
            end
            write(out, SubString(str, wpos))
            String(take!(out))
        end
    end
end

@generated function replace_gen(str::String, count::Int, pat_repls::Pair...)
    replace_gen_impl(str, count, pat_repls...)
end

# consolidate patterns
# collections of chars are transformed to Char if collection has only one member
# strings of length 1 are transformed to the character
# single char x is transformed to `equalto(x)`
# abstract strings are converted to strings
# character collection pat is transformed to `occursin(pat)`
# `occursin(character collection)` is transformed like occursin().x
# `occursin(empty string)` is ignored
# Regex and Callables are unchanged
# other patterns are stringified (TODO should be rejected!)
#
function consol_pattern(pat::Union{Tuple{Vararg{Char}},AbstractVector{Char},Set{Char}})
    if length(pat) > 1
        occursin(pat)
    elseif length(pat) == 1
        equalto(next(pat, start(pat))[1]) # works also for `Set{Char}` as opposed to `first`
    else
        isempty # case of empty collection - `isempty(::Char) == false`
    end
end
function consol_pattern(pat::Base.OccursIn{<:Union{Tuple{Vararg{Char}},AbstractVector{Char},Set{Char}}})
    consol_pattern(pat.x)
end
function consol_pattern(pat::Base.OccursIn{<:AbstractString})
    isempty(pat.x) ? isempty : pat
end
consol_pattern(pat::Char) = equalto(pat)
consol_pattern(pat::AbstractString) = length(pat) == 1 ? equalto(pat[1]) : string(pat)
consol_pattern(pat::Union{Regex,Callable}) = pat
consol_pattern(pat) = string(pat)

# consolidate replacements
# SubstitutionString, Char, and functions are unchanged
# strings of length 1 are transformed to the character
# abstract strings are converted to strings
# other replacements are stringified (TODO should be rejected?)
#
consol_repl(repl::Union{Char,SubstitutionString,Callable}) = repl
consol_repl(repl::AbstractString) = length(repl) == 1 ? repl[1] : string(repl)
consol_repl(repl) = string(repl)

# check if pattern is equal to replacement (no-op replacement)
is_modify(p::Pair{Base.EqualTo{Char},Char}) = p.first.x != p.second
is_modify(p::Pair{typeof(isempty)}) = false
is_modify(p::Pair) = p.first != p.second

# replace collection of Char with function that checks occurrence in this collection
function pat_repl_pair(p::Pair{<:TIF,<:TIS})::Pair{<:TF,<:TS}
    pat::TF  = consol_pattern(p.first)
    repl::TS = consol_repl(p.second)
    repl isa SubstitutionString && !(pat isa Regex) && error("substitution string requires regex")
    repl = funtostring(pat, repl)
    pat === p.first && repl === p.second ? p : pat => repl
end

funtostring(pat::Base.EqualTo{Char}, fun::Callable) = stringfun(fun, pat.x)
funtostring(pat::String, fun::Callable) = stringfun(fun, pat)
funtostring(::Any, y) = y

function stringfun(fun::Function, s::Union{Char,String})
    if type_hasmethod(typeof(fun), (Char,)) && length(s) == 1
        fun(s[1])
    else
        fun(string(s))
    end
end

function check_args(str::String, count::Int, pat_repls::Pair...)
    count < 0 && throw(DomainError(count, "`count` must be non-negative."))
    count == 0 || length(pat_repls) == 0
end

function consolidate_args(pat_repls::Pair...)
    filter!(is_modify, collect(Pair, pat_repl_pair.(pat_repls)))
end

function replace_code(str::String, pat_repls::Pair...; count::Int=typemax(Int))
    check_args(str, count, pat_repls...) && return :str
    pat_repls2 = consolidate_args(pat_repls...)
    length(pat_repls2) == 0 && return :str
    ReplaceImpl.replace_gen_impl(typeof(str), Int, typeof.(pat_repls2)...)
end

end # module ReplaceImpl

# cover the multiple pairs case
function replace(str::AbstractString, pat_repls::Pair...; count::Integer=typemax(Int))
    count = Int(clamp(count, typemin(Int), typemax(Int)))
    ReplaceImpl.check_args(str, count, pat_repls...) && return str
    pat_repls2 = ReplaceImpl.consolidate_args(pat_repls...)
    length(pat_repls2) == 0 && return str
    ReplaceImpl.replace_gen(String(str), count, pat_repls2...)
end

