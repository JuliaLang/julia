function closer(ps::ParseState)
    (ps.closer.newline && ps.ws.kind == NewLineWS && ps.t.kind != Tokens.COMMA) ||
    (ps.closer.semicolon && ps.ws.kind == SemiColonWS) ||
    (isoperator(ps.nt) && precedence(ps.nt) <= ps.closer.precedence) ||
    (ps.nt.kind == Tokens.WHERE && ps.closer.precedence == LazyAndOp) ||
    (ps.closer.inwhere && ps.nt.kind == Tokens.WHERE) ||
    (ps.closer.precedence > WhereOp && (
        ps.nt.kind == Tokens.LPAREN ||
        ps.nt.kind == Tokens.LBRACE ||
        ps.nt.kind == Tokens.LSQUARE ||
        (ps.nt.kind == Tokens.STRING && isemptyws(ps.ws)) ||
        ((ps.nt.kind == Tokens.RPAREN || ps.nt.kind == Tokens.RSQUARE) && isidentifier(ps.nt))  
    )) ||
    (ps.nt.kind == Tokens.COMMA && ps.closer.precedence > 0) ||
    ps.nt.kind == Tokens.ENDMARKER ||
    (ps.closer.comma && iscomma(ps.nt)) ||
    (ps.closer.tuple && (iscomma(ps.nt) || isassignment(ps.nt))) ||
    (ps.nt.kind == Tokens.FOR && ps.closer.precedence > -1) ||
    (ps.closer.block && ps.nt.kind == Tokens.END) ||
    (ps.closer.paren && ps.nt.kind == Tokens.RPAREN) ||
    (ps.closer.brace && ps.nt.kind == Tokens.RBRACE) ||
    (ps.closer.square && ps.nt.kind == Tokens.RSQUARE) ||
    ps.nt.kind == Tokens.ELSEIF || 
    ps.nt.kind == Tokens.ELSE ||
    ps.nt.kind == Tokens.CATCH || 
    ps.nt.kind == Tokens.FINALLY || 
    (ps.closer.ifop && isoperator(ps.nt) && (precedence(ps.nt) <= 0 || ps.nt.kind == Tokens.COLON)) ||
    (ps.closer.range && (ps.nt.kind == Tokens.FOR || iscomma(ps.nt) || ps.nt.kind == Tokens.IF)) ||
    (ps.closer.ws && !isemptyws(ps.ws) &&
        !(ps.nt.kind == Tokens.COMMA) &&
        !(ps.t.kind == Tokens.COMMA) &&
        !(!ps.closer.inmacro && ps.nt.kind == Tokens.FOR) &&
        !(ps.nt.kind == Tokens.DO) &&
        !(
            (isbinaryop(ps.nt) && !(isemptyws(ps.nws) && isunaryop(ps.nt) && ps.closer.wsop)) || 
            (isunaryop(ps.t) && ps.ws.kind == WS)
        )) ||
    (ps.nt.startbyte ≥ ps.closer.stop) ||
    (ps.closer.unary && (ps.t.kind in (Tokens.INTEGER, Tokens.FLOAT, Tokens.RPAREN, Tokens.RSQUARE,Tokens.RBRACE) && ps.nt.kind == Tokens.IDENTIFIER)) ||
    ps.errored
end

"""
    @closer ps rule body

Continues parsing closing on `rule`.
"""
macro closer(ps, opt, body)
    quote
        local tmp1 = $(esc(ps)).closer.$opt
        $(esc(ps)).closer.$opt = true
        out = $(esc(body))
        $(esc(ps)).closer.$opt = tmp1
        out
    end
end

macro closeparen(ps, body)
    quote
        local tmp1 = $(esc(ps)).closer.paren
        $(esc(ps)).closer.paren = true
        push!($(esc(ps)).closer.cc, :paren)
        out = $(esc(body))
        pop!($(esc(ps)).closer.cc)
        $(esc(ps)).closer.paren = tmp1
        out
    end
end

macro closesquare(ps, body)
    quote
        local tmp1 = $(esc(ps)).closer.square
        $(esc(ps)).closer.square = true
        push!($(esc(ps)).closer.cc, :square)
        out = $(esc(body))
        pop!($(esc(ps)).closer.cc)
        $(esc(ps)).closer.square = tmp1
        out
    end
end
macro closebrace(ps, body)
    quote
        local tmp1 = $(esc(ps)).closer.brace
        $(esc(ps)).closer.brace = true
        push!($(esc(ps)).closer.cc, :brace)
        out = $(esc(body))
        pop!($(esc(ps)).closer.cc)
        $(esc(ps)).closer.brace = tmp1
        out
    end
end

"""
    @nocloser ps rule body

Continues parsing not closing on `rule`.
"""
macro nocloser(ps, opt, body)
    quote
        local tmp1 = $(esc(ps)).closer.$opt
        $(esc(ps)).closer.$opt = false
        out = $(esc(body))
        $(esc(ps)).closer.$opt = tmp1
        out
    end
end

"""
    @precedence ps prec body

Continues parsing binary operators until it hits a more loosely binding
operator (with precdence lower than `prec`).
"""
macro precedence(ps, prec, body)
    quote
        local tmp1 = $(esc(ps)).closer.precedence
        $(esc(ps)).closer.precedence = $(esc(prec))
        out = $(esc(body))
        $(esc(ps)).closer.precedence = tmp1
        out
    end
end


# Closer_TMP and ancillary functions help reduce code generation
struct Closer_TMP
    newline::Bool
    semicolon::Bool
    inmacro::Bool
    tuple::Bool
    comma::Bool
    insquare::Bool
    range::Bool
    ifelse::Bool
    ifop::Bool
    ws::Bool
    wsop::Bool
    unary::Bool
    precedence::Int
end

@noinline function create_tmp(c::Closer)
    Closer_TMP(
        c.newline,
        c.semicolon,
        c.inmacro,
        c.tuple,
        c.comma,
        c.insquare,
        c.range,
        c.ifelse,
        c.ifop,
        c.ws,
        c.wsop,
        c.unary,
        c.precedence
    )
end

@noinline function update_from_tmp!(c::Closer, tmp::Closer_TMP)
    c.newline = tmp.newline
    c.semicolon = tmp.semicolon
    c.inmacro = tmp.inmacro
    c.tuple = tmp.tuple
    c.comma = tmp.comma
    c.insquare = tmp.insquare
    c.range = tmp.range
    c.ifelse = tmp.ifelse
    c.ifop = tmp.ifop
    c.ws = tmp.ws
    c.wsop = tmp.wsop
    c.unary = tmp.unary
    c.precedence = tmp.precedence
end


@noinline function update_to_default!(c::Closer)
    c.newline = true
    c.semicolon = true
    c.inmacro = false
    c.tuple = false
    c.comma = false
    c.insquare = false
    c.range = false
    c.ifelse = false
    c.ifop = false
    c.ws = false
    c.wsop = false
    c.unary = false
    c.precedence = -1
end


"""
    @default ps body

Parses the next expression using default closure rules.
"""
macro default(ps, body)
    quote
        TMP = create_tmp($(esc(ps)).closer)
        update_to_default!($(esc(ps)).closer)
        out = $(esc(body))
        update_from_tmp!($(esc(ps)).closer, TMP)
        out
    end
end


isidentifier(t::AbstractToken) = t.kind == Tokens.IDENTIFIER

isliteral(t::AbstractToken) = Tokens.begin_literal < t.kind < Tokens.end_literal

isbool(t::AbstractToken) =  Tokens.TRUE ≤ t.kind ≤ Tokens.FALSE
iscomma(t::AbstractToken) =  t.kind == Tokens.COMMA

iskw(t::AbstractToken) = Tokens.iskeyword(t.kind)

isinstance(t::AbstractToken) = isidentifier(t) ||
                       isliteral(t) ||
                       isbool(t) ||
                       iskw(t)


ispunctuation(t::AbstractToken) = t.kind == Tokens.COMMA ||
                          t.kind == Tokens.END ||
                          Tokens.LSQUARE ≤ t.kind ≤ Tokens.RPAREN || 
                          t.kind == Tokens.AT_SIGN

isstring(x) = false
isstring(x::EXPR{StringH}) = true
isstring(x::LITERAL) = x.kind == Tokens.STRING || x.kind == Tokens.TRIPLE_STRING
is_integer(x) = x isa LITERAL && x.kind == Tokens.INTEGER
is_float(x) = x isa LITERAL && x.kind == Tokens.FLOAT
is_number(x) = x isa LITERAL && (x.kind == Tokens.INTEGER || x.kind == Tokens.FLOAT)
is_nothing(x) = x isa LITERAL && x.kind == Tokens.NOTHING

isajuxtaposition(ps::ParseState, ret) = ((is_number(ret) && (ps.nt.kind == Tokens.IDENTIFIER || ps.nt.kind == Tokens.LPAREN || ps.nt.kind == Tokens.CMD || ps.nt.kind == Tokens.STRING || ps.nt.kind == Tokens.TRIPLE_STRING)) || 
        ((ret isa UnarySyntaxOpCall && is_prime(ret.arg2) && ps.nt.kind == Tokens.IDENTIFIER) ||
        ((ps.t.kind == Tokens.RPAREN || ps.t.kind == Tokens.RSQUARE) && (ps.nt.kind == Tokens.IDENTIFIER || ps.nt.kind == Tokens.CMD)) ||
        ((ps.t.kind == Tokens.STRING || ps.t.kind == Tokens.TRIPLE_STRING) && (ps.nt.kind == Tokens.STRING || ps.nt.kind == Tokens.TRIPLE_STRING)))) || ((ps.t.kind in (Tokens.INTEGER, Tokens.FLOAT) || ps.t.kind in (Tokens.RPAREN,Tokens.RSQUARE,Tokens.RBRACE)) && ps.nt.kind == Tokens.IDENTIFIER)


# Testing functions



function test_order(x, out = [])
    if x isa EXPR
        for y in x
            test_order(y, out)
        end
    else
        push!(out, x)
    end
    out
end

function test_find(str)
    x = parse(str, true)
    for i = 1:sizeof(str)
        _find(x, i)
    end
end

# When using the FancyDiagnostics package, Base.parse, is the
# same as CSTParser.parse. Manually call the flisp parser here
# to make sure we test what we want, even when people load the
# FancyDiagnostics package.
function flisp_parse(str::AbstractString, pos::Int; greedy::Bool=true, raise::Bool=true)
    # pos is one based byte offset.
    # returns (expr, end_pos). expr is () in case of parse error.
    bstr = String(str)
    ex, pos = ccall(:jl_parse_string, Any,
                    (Ptr{UInt8}, Csize_t, Int32, Int32),
                    bstr, sizeof(bstr), pos-1, greedy ? 1 : 0)
    if raise && isa(ex,Expr) && ex.head === :error
        throw(Meta.ParseError(ex.args[1]))
    end
    if ex === ()
        raise && throw(Meta.ParseError("end of input"))
        ex = Expr(:error, "end of input")
    end
    return ex, pos+1 # C is zero-based, Julia is 1-based
end

function flisp_parse(str::AbstractString; raise::Bool=true)
    ex, pos = flisp_parse(str, 1, greedy=true, raise=raise)
    if isa(ex,Expr) && ex.head === :error
        return ex
    end
    if !(pos > ncodeunits(str))
        raise && throw(Meta.ParseError("extra token after end of expression"))
        return Expr(:error, "extra token after end of expression")
    end
    return ex
end

function flisp_parse(stream::IO; greedy::Bool = true, raise::Bool = true)
    pos = position(stream)
    ex, Δ = flisp_parse(read(stream, String), 1, greedy = greedy, raise = raise)
    seek(stream, pos + Δ - 1)
    return ex
end

using Base.Meta
norm_ast(a::Any) = begin
    if isa(a, Expr)
        for (i, arg) in enumerate(a.args)
            a.args[i] = norm_ast(arg)
        end
        if a.head === :line
            return Expr(:line, a.args[1], :none)
        end
        if a.head === :macrocall
            fa = a.args[1]
            if fa === Symbol("@int128_str")
                return Base.parse(Int128,a.args[3])
            elseif fa === Symbol("@uint128_str")
                return Base.parse(UInt128,a.args[3])
            elseif fa === Symbol("@bigint_str")
                return  Base.parse(BigInt,a.args[3])
            elseif fa == Symbol("@big_str")
                s = a.args[2]
                n = tryparse(BigInt,s)
                if !(n == nothing)
                    return get(n)
                end
                n = tryparse(BigFloat,s)
                if !(n == nothing)
                    return isnan(get(n)) ? :NaN : get(n)
                end
                return s
            end
        elseif length(a.args) >= 2 && isexpr(a, :call) && a.args[1] == :- && isa(a.args[2], Number)
            return -a.args[2]
        end
        return a
    elseif isa(a, QuoteNode)
        return Expr(:quote, norm_ast(a.value))
    elseif isa(a, AbstractFloat) && isnan(a)
        return :NaN
    end
    return a
end

function flisp_parsefile(str, display = true)
    io = IOBuffer(str)
    failed = false
    x1 = Expr(:file)
    try
        while !eof(io)
            push!(x1.args, flisp_parse(io))
        end
    catch er
        isa(er, InterruptException) && rethrow(er)
        if display
            Base.showerror(stdout, er, catch_backtrace())
            println()
        end
        return x1, true
    end
    if length(x1.args) > 0  && x1.args[end] == nothing
        pop!(x1.args)
    end
    x1 = norm_ast(x1)
    remlineinfo!(x1)
    return x1, false
end

function cst_parsefile(str)
    x, ps = CSTParser.parse(ParseState(str), true)
    sp = check_span(x)
    # remove leading/trailing nothings
    if length(x.args) > 0 && is_nothing(x.args[1])
        popfirst!(x.args)
    end
    if length(x.args) > 0 && is_nothing(x.args[end])
        pop!(x.args)
    end
    x0 = norm_ast(Expr(x))
    x0, ps.errored, sp
end

function check_file(file, ret, neq)
    str = read(file, String)
    x0, cstfailed, sp = cst_parsefile(str)
    x1, flispfailed = flisp_parsefile(str)
    
    print("\r                             ")
    if !isempty(sp)
        printstyled(file, color = :blue)
        @show sp
        println()
        push!(ret, (file, :span))
    end
    if cstfailed
        printstyled(file, color = :yellow)
        println()
        push!(ret, (file, :errored))
    elseif !(x0 == x1)
        cumfail = 0
        printstyled(file, color = :green)
        println()
        c0, c1 = CSTParser.compare(x0, x1)
        printstyled(string("    ", c0), bold = true, color = :ligth_red)
        println()
        printstyled(string("    ", c1), bold = true, color = :light_green)
        println()
        push!(ret, (file, :noteq))
    end    
end

function check_base(dir = dirname(Base.find_source_file("essentials.jl")), display = false)
    N = 0
    neq = 0
    err = 0
    aerr = 0
    fail = 0
    bfail = 0
    ret = []
    oldstderr = stderr
    redirect_stderr()
    for (rp, d, files) in walkdir(dir)
        for f in files
            file = joinpath(rp, f)
            if endswith(file, ".jl")
                N += 1
                try
                    print("\r", rpad(string(N), 5), rpad(string(round(fail / N * 100, sigdigits = 3)), 8), rpad(string(round(err / N * 100, sigdigits = 3)), 8), rpad(string(round(neq / N * 100, sigdigits = 3)), 8))
                    
                    check_file(file, ret, neq)
                catch er
                    isa(er, InterruptException) && rethrow(er)
                    if display
                        Base.showerror(stdout, er, catch_backtrace())
                        println()
                    end
                    fail += 1
                    printstyled(file, color = :red)
                    println()
                    push!(ret, (file, :failed))
                end
            end
        end
    end
    redirect_stderr(oldstderr)
    if bfail + fail + err + neq > 0
        println("\r$N files")
        printstyled("failed", color = :red)
        println(" : $fail    $(100*fail/N)%")
        printstyled("errored", color = :yellow)
        println(" : $err     $(100*err/N)%")
        printstyled("not eq.", color = :green)
        println(" : $neq    $(100*neq/N)%", "  -  $aerr     $(100*aerr/N)%")
        printstyled("base failed", color = :magenta)
        println(" : $bfail    $(100*bfail/N)%")
    end
    ret
end

"""
    compare(x,y)

Recursively checks whether two Base.Expr are the same. Returns unequal sub-
expressions.
"""
compare(x, y) = x == y ? true : (x, y)

function compare(x::Expr, y::Expr)
    if x == y
        return true
    else
        if x.head != y.head
            return (x, y)
        end
        if length(x.args) != length(y.args)
            return (x.args, y.args)
        end
        for i = 1:length(x.args)
            t = compare(x.args[i], y.args[i])
            if t != true
                return t
            end
        end
    end
end

"""
check_span(x, neq = [])

Recursively checks whether the span of an expression equals the sum of the span
of its components. Returns a vector of failing expressions.
"""
function check_span(x::EXPR{StringH}, neq = []) end
function check_span(x::T, neq = []) where T <: Union{IDENTIFIER,LITERAL,OPERATOR,KEYWORD,PUNCTUATION} neq end

function check_span(x::UnaryOpCall, neq = []) 
    check_span(x.op)
    check_span(x.arg)
    if x.op.fullspan + x.arg.fullspan != x.fullspan
        push!(neq, x)
    end
    neq
end
function check_span(x::UnarySyntaxOpCall, neq = []) 
    check_span(x.arg1)
    check_span(x.arg2)
    if x.arg1.fullspan + x.arg2.fullspan != x.fullspan
        push!(neq, x)
    end
    neq
end

function check_span(x::T, neq = []) where T <: Union{BinaryOpCall,BinarySyntaxOpCall}
    check_span(x.arg1)
    check_span(x.op)
    check_span(x.arg2)
    if x.arg1.fullspan + x.op.fullspan + x.arg2.fullspan != x.fullspan
        push!(neq, x)
    end
    neq
end

function check_span(x::WhereOpCall, neq = [])
    check_span(x.arg1)
    check_span(x.op)
    for a in x.args
        check_span(a)
    end
    if x.arg1.fullspan + x.op.fullspan + sum(a.fullspan for a in x.args) != x.fullspan
        push!(neq, x)
    end
    neq
end

function check_span(x::ConditionalOpCall, neq = [])
    check_span(x.cond)
    check_span(x.op1)
    check_span(x.arg1)
    check_span(x.op2)
    check_span(x.arg2)
    if x.cond.fullspan + x.op1.fullspan + x.arg1.fullspan + x.op2.fullspan + x.arg2.fullspan != x.fullspan
        push!(neq, x)
    end
    neq
end


function check_span(x::EXPR, neq = [])
    s = 0
    for a in x.args
        check_span(a, neq)
        s += a.fullspan
    end
    if length(x.args) > 0 && s != x.fullspan
        push!(neq, x)
    end
    neq
end

function speed_test()
    dir = dirname(Base.find_source_file("essentials.jl"))
    println("speed test : ", @timed(for i = 1:5
    parse(read(joinpath(dir, "essentials.jl"), String), true);
    parse(read(joinpath(dir, "abstractarray.jl"), String), true);
end)[2])
end

"""
    check_reformat()

Reads and parses all files in current directory, applys formatting fixes and checks that the output AST remains the same.
"""
function check_reformat()
    fs = filter(f -> endswith(f, ".jl"), readdir())
    for (i, f) in enumerate(fs)
        f == "deprecated.jl" && continue
        str = read(f, String)
        x, ps = parse(ParseState(str), true);
        cnt = 0
        for i = 1:length(x) - 1
            y = x[i]
            sstr = str[cnt + (1:y.span)]

            y1 = parse(sstr)
            @assert Expr(y) == Expr(y1)
            cnt += y.span
        end
    end
end


function trailing_ws_length(x)
    x.fullspan - x.span
end




Base.iterate(x::EXPR) = length(x) == 0 ? nothing : (x.args[1], 1)
Base.iterate(x::EXPR, s) = s < length(x) ? (x.args[s + 1], s + 1) : nothing
Base.length(x::EXPR) = length(x.args)

Base.iterate(x::UnaryOpCall) = x.op, 1
Base.iterate(x::UnaryOpCall, s) = s == 1 ? (x.arg, 2) : nothing
Base.length(x::UnaryOpCall) = 2

Base.iterate(x::UnarySyntaxOpCall) = x.arg1, 1
Base.iterate(x::UnarySyntaxOpCall, s) = s == 1 ? (x.arg2, 2) : nothing
Base.length(x::UnarySyntaxOpCall) = 2

Base.iterate(x::BinarySyntaxOpCall) = x.arg1, 1
Base.iterate(x::BinarySyntaxOpCall, s) = s > 2 ? nothing : (getfield(x, s + 1), s + 1)
Base.length(x::BinarySyntaxOpCall) = 3

Base.iterate(x::BinaryOpCall) = x.arg1, 1
Base.iterate(x::BinaryOpCall, s) = s > 2 ? nothing : (getfield(x, s + 1), s + 1)
Base.length(x::BinaryOpCall) = 3

Base.iterate(x::WhereOpCall) = x.arg1, 1
function Base.iterate(x::WhereOpCall, s) 
    if s == 1
        return x.op, 2
    elseif s < length(x)
        return x.args[s - 1] , s + 1
    end
end
Base.length(x::WhereOpCall) = 2 + length(x.args)

Base.iterate(x::ConditionalOpCall) = x.cond, 1
Base.iterate(x::ConditionalOpCall, s) = s < length(x) ? (getfield(x, s + 1), s + 1) : nothing
Base.length(x::ConditionalOpCall) = 5

for t in (CSTParser.IDENTIFIER, CSTParser.OPERATOR, CSTParser.LITERAL, CSTParser.PUNCTUATION, CSTParser.KEYWORD)
    Base.iterate(x::t) = nothing
    Base.iterate(x::t, s) = nothing
    Base.length(x::t) = 0
    Base.isiterable(x::t) = false
end

@inline val(token::RawToken, ps::ParseState) = String(ps.l.io.data[token.startbyte+1:token.endbyte+1])

str_value(x) = ""
str_value(x::T) where T <: Union{IDENTIFIER,LITERAL} = x.val
str_value(x::OPERATOR) = string(Expr(x))
str_value(x::EXPR{MacroName}) = string(Expr(x))

_unescape_string(s::AbstractString) = sprint(_unescape_string, s, sizehint=lastindex(s))
function _unescape_string(io, s::AbstractString)
    a = Iterators.Stateful(s)
    for c in a
        if !isempty(a) && c == '\\'
            c = popfirst!(a)
            if c == 'x' || c == 'u' || c == 'U'
                n = k = 0
                m = c == 'x' ? 2 :
                    c == 'u' ? 4 : 8
                while (k+=1) <= m && !isempty(a)
                    nc = Base.peek(a)
                    n = '0' <= nc <= '9' ? n<<4 + nc-'0' :
                        'a' <= nc <= 'f' ? n<<4 + nc-'a'+10 :
                        'A' <= nc <= 'F' ? n<<4 + nc-'A'+10 : break
                    popfirst!(a)
                end
                if k == 1
                    # throw(ArgumentError("invalid $(m == 2 ? "hex (\\x)" :
                    #                         "unicode (\\u)") escape sequence used in $(repr(s))"))
                    # push error to ParseState?
                    n = 0
                end
                if m == 2 # \x escape sequence
                    write(io, UInt8(n))
                else
                    print(io, Char(n))
                end
            elseif '0' <= c <= '7'
                k = 1
                n = c-'0'
                while (k+=1) <= 3 && !isempty(a)
                    c  = Base.peek(a)
                    n = ('0' <= c <= '7') ? n<<3 + c-'0' : break
                    popfirst!(a)
                end
                if n > 255
                    # throw(ArgumentError("octal escape sequence out of range"))
                    # push error to ParseState?
                    n = 255
                end
                write(io, UInt8(n))
            else
                print(io, c == 'a' ? '\a' :
                          c == 'b' ? '\b' :
                          c == 't' ? '\t' :
                          c == 'n' ? '\n' :
                          c == 'v' ? '\v' :
                          c == 'f' ? '\f' :
                          c == 'r' ? '\r' :
                          c == 'e' ? '\e' : c)
            end
        else
            print(io, c)
        end
    end
end


