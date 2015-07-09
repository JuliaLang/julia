# This file is a part of Julia. License is MIT: http://julialang.org/license

module Meta
#
# convenience functions for metaprogramming
#

export quot,
       isexpr,
       show_sexpr,
       isline,
       rmlines,
       unblock,
       namify,
       uncurly,
       isdef,
       subs,
       @expand,
       @match

quot(ex) = Expr(:quote, ex)

isexpr(ex::Expr, head)          = ex.head === head
isexpr(ex::Expr, heads::Set)    = in(ex.head, heads)
isexpr(ex::Expr, heads::Vector) = in(ex.head, heads)
isexpr(ex,       head)          = false

isexpr(ex,       head, n::Int)  = isexpr(ex, head) && length(ex.args) == n

isline(ex) = isexpr(ex, :line) || isa(ex, LineNumberNode)

rmlines(xs) = filter(x->!isline(x), xs)
rmlines(x::Expr) = Expr(x.head, rmlines(x.args)...)

function unblock(ex)
    isexpr(ex, :block) || return ex
    exs = filter(ex->!isline(ex), ex.args)
    length(exs) == 1 || return ex
    return unblock(exs[1])
end

namify(ex::Expr) = isexpr(ex, :.) ? ex : namify(ex.args[1])
namify(ex::QuoteNode) = ex.value
namify(sy::Symbol) = sy

uncurly(ex) = isexpr(ex, :curly) ? ex.args[1] : ex

isdef(ex) = isexpr(ex, :function) || (isexpr(ex, :(=)) && isexpr(ex.args[1], :call))

subs(ex::Expr, s, s′) = ex == s ? s′ : Expr(ex.head, map(ex -> subs(ex, s, s′), ex.args)...)

subs(ex, s, s′) = ex == s ? s′ : ex

macro expand(ex)
      :(macroexpand($(Expr(:quote, ex))))
end

# ---- show_sexpr: print an AST as an S-expression ----

show_sexpr(ex) = show_sexpr(STDOUT, ex)
show_sexpr(io::IO, ex) = show_sexpr(io, ex, 0)
show_sexpr(io::IO, ex, indent::Int) = show(io, ex)

const sexpr_indent_width = 2

function show_sexpr(io::IO, ex::QuoteNode, indent::Int)
    inner = indent + sexpr_indent_width
    print(io, "(:quote, #QuoteNode\n", " "^inner)
    show_sexpr(io, ex.value, inner)
    print(io, '\n', " "^indent, ')')
end
function show_sexpr(io::IO, ex::Expr, indent::Int)
    inner = indent + sexpr_indent_width
    print(io, '(')
    show_sexpr(io, ex.head, inner)
    for arg in ex.args
        print(io, ex.head === :block ? ",\n"*" "^inner : ", ")
        show_sexpr(io, arg, inner)
    end
    if length(ex.args) == 0; print(io, ",)")
    else print(io, (ex.head === :block ? "\n"*" "^indent : ""), ')')
    end
end

# Expression Matching

type MatchError
    pat
    ex
end

nomatch(pat, ex) = throw(MatchError(pat, ex))

isbinding(s) = false
isbinding(s::Symbol) = Base.ismatch(r"[^_]_(_str)?$", string(s))

bname(s::Symbol) = symbol(Base.match(r"^@?(\w*?)_+", string(s)).captures[1])

match_inner(pat, ex, env) = (pat == ex ? env : nomatch(pat, ex))

isslurp(s) = false
isslurp(s::Symbol) = s == :__ || Base.ismatch(r"[^_]__$", string(s))

function slurprange(pat)
    slurps = length(filter(isslurp, pat))
    slurps == 0 && return 0,0
    slurps > 1 && error("Pattern may only contain one slurp.")

    left, right = 1, 1
    while !isslurp(pat[left]) left += 1 end
    while !isslurp(pat[end+1-right]) right += 1 end
    return left, right
end

inrange(i, range, len) =
    range ≠ (0,0) && i ≥ range[1] && i ≤ len+1-range[2]

function match_inner(pat::Expr, ex::Expr, env)
    match(pat.head, ex.head, env)
    pat, ex = rmlines(pat), rmlines(ex)
    sr = slurprange(pat.args)
    slurp = Any[]
    i = 1
    for p in pat.args
        i > length(ex.args) &&
            (isslurp(p) ? (env[bname(p)] = slurp) : nomatch(pat, ex))

        while inrange(i, sr, length(ex.args))
            push!(slurp, ex.args[i])
            i += 1
        end

        if isslurp(p)
            p ≠ :__ && (nv[bname(p)] = slurp)
        else
            match(p, ex.args[i], env)
            i += 1
        end
    end
    i == length(ex.args)+1 || nomatch(pat, ex)
    return env
end

blockunify(a, b) =
    isexpr(a, :block) && !isexpr(b, :block) ? (a, Expr(:block, b)) :
    !isexpr(a, :block) && isexpr(b, :block) ? (Expr(:block, a), b) :
    (a, b)

function normalise(ex)
    ex = unblock(ex)
    isa(ex, QuoteNode) && (ex = Expr(:quote, ex.value))
    return ex
end

function match(pat, ex, env)
    pat, ex = normalise(pat), normalise(ex)
    pat == :_ && return env
    isbinding(pat) && return (env[bname(pat)] = ex; env)
    pat, ex = blockunify(pat, ex)
    return match_inner(pat, ex, env)
end

match(pat, ex) = match(pat, ex, Dict())

function trymatch(pat, ex)
    try match(pat, ex)
    catch e isa(e, MatchError) ? nothing : rethrow()
    end
end

# @match macro

allbindings(pat, bs) =
    isbinding(pat) || (isslurp(pat) && pat ≠ :__) ? push!(bs, bname(pat)) :
    isexpr(pat, :$) ? bs :
    isa(pat, Expr) ? map(pat -> allbindings(pat, bs), [pat.head, pat.args...]) :
    bs

allbindings(pat) = (bs = Any[]; allbindings(pat, bs); bs)

function bindinglet(bs, body)
    ex = :(let $(esc(:env)) = env
               $body
           end)
    for b in bs
        push!(ex.args, :($(esc(b)) = env[$(Expr(:quote, b))]))
    end
    return ex
end

function makeclause(line, els = nothing)
    env = trymatch(:(pat_ -> yes_), line)
    env == nothing && error("Invalid match clause $line")
    pat, yes = env[:pat], env[:yes]
    quote
        env = trymatch($(Expr(:quote, pat)), ex)
        if env != nothing
            $(bindinglet(allbindings(pat), esc(yes)))
        else
            $els
        end
    end
end

macro match(ex, lines)
    @assert isexpr(lines, :block)
    result = quote
        ex = $(esc(ex))
    end
    body = nothing
    for line in reverse(rmlines(lines).args)
        isline(result) && push!(result, line)
        body = makeclause(line, body)
    end
    push!(result.args, body)
    return result
end

end # module
