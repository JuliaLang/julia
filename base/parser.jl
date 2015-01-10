# Julia Source Parser
module Parser

using ..Lexer

typealias CharSymbol Union(Char, Symbol)

type ParseState
    # disable range colon for parsing ternary cond op
    range_colon_enabled::Bool

    # in space sensitvie mode "x -y" is 2 exprs, not subtraction
    space_sensitive::Bool

    # treat "end" like a normal symbol, instead of a reserved word
    inside_vector::Bool

    # treat newline like ordinary whitespace instead of a reserved word
    end_symbol::Bool

    # treat newline like ordinary whitespace instead of as a potential separator
    whitespace_newline::Bool
end

ParseState() = ParseState(true, false, false, false, false)

peek_token(ps::ParseState, ts::TokenStream)    = Lexer.peek_token(ts, ps.whitespace_newline)
next_token(ps::ParseState, ts::TokenStream)    = Lexer.next_token(ts, ps.whitespace_newline)
require_token(ps::ParseState, ts::TokenStream) = Lexer.require_token(ts, ps.whitespace_newline)

macro with_normal_ops(ps, body)
    quote
        local tmp1 = $(esc(ps)).range_colon_enabled
        local tmp2 = $(esc(ps)).space_sensitive
        try
            $(esc(ps)).range_colon_enabled = true
            $(esc(ps)).space_sensitive     = false
            $(esc(body))
        finally
            $(esc(ps)).range_colon_enabled = tmp1
            $(esc(ps)).space_sensitive     = tmp2
        end
    end
end

macro without_range_colon(ps, body)
    quote
        local tmp1 = $(esc(ps)).range_colon_enabled
        try
            $(esc(ps)).range_colon_enabled = false
            $(esc(body))
        finally
            $(esc(ps)).range_colon_enabled = tmp1
        end
    end
end

macro with_inside_vec(ps, body)
    quote
        local tmp1 = $(esc(ps)).space_sensitive
        local tmp2 = $(esc(ps)).inside_vector
        local tmp3 = $(esc(ps)).whitespace_newline
        try
            $(esc(ps)).space_sensitive = true
            $(esc(ps)).inside_vector   = true
            $(esc(ps)).whitespace_newline = false
            $(esc(body))
        finally
            $(esc(ps)).space_sensitive = tmp1
            $(esc(ps)).inside_vector   = tmp2
            $(esc(ps)).whitespace_newline = tmp3
        end
    end
end

macro with_end_symbol(ps, body)
    quote
        local tmp1 = $(esc(ps)).end_symbol
        try
            $(esc(ps)).end_symbol = true
            $(esc(body))
        finally
            $(esc(ps)).end_symbol = tmp1
        end
    end
end

macro with_whitespace_newline(ps, body)
    quote
        local tmp1 = $(esc(ps)).whitespace_newline
        try
            $(esc(ps)).whitespace_newline = true
            $(esc(body))
        finally
            $(esc(ps)).whitespace_newline = tmp1
        end
    end
end

macro without_whitespace_newline(ps, body)
    quote
        local tmp1 = $(esc(ps)).whitespace_newline
        try
            $(esc(ps)).whitespace_newline = false
            $(esc(body))
        finally
            $(esc(ps)).whitespace_newline = tmp1
        end
    end
end

macro space_sensitive(ps, body)
    quote
        local tmp1 = $(esc(ps)).space_sensitive
        local tmp2 = $(esc(ps)).whitespace_newline
        try
            $(esc(ps)).space_sensitive = true
            $(esc(ps)).whitespace_newline = false
            $(esc(body))
        finally
            $(esc(ps)).space_sensitive    = tmp1
            $(esc(ps)).whitespace_newline = tmp2
        end
    end
end

curline(ts::TokenStream)  = ts.lineno
filename(ts::TokenStream) = symbol("none")

line_number_node(ts) = LineNumberNode(curline(ts))
line_number_filename_node(ts::TokenStream) = Expr(:line, curline(ts), filename(ts))

# insert line/file for short form function defs, otherwise leave alone
function short_form_function_loc(ex, lno, filename)
    if isa(ex, Expr) && ex.head === :(=) && isa(ex.args[1], Expr) && ex.args[1].head === :call
       block = Expr(:block, Expr(:line, lno, filename))
       append!(block.args, ex.args[2:end])
       return Expr(:(=), ex.args[1], block)
   end
   return ex
end

const SYM_DO      = symbol("do")
const SYM_ELSE    = symbol("else")
const SYM_ELSEIF  = symbol("elseif")
const SYM_END     = symbol("end")
const SYM_CATCH   = symbol("catch")
const SYM_FINALLY = symbol("finally")
const SYM_SQUOTE  = symbol("'")

const EOF = Lexer.EOF

const is_invalid_initial_token = let invalid = Set([')', ']', '}', SYM_ELSE, SYM_ELSEIF, SYM_CATCH, SYM_FINALLY])
    is_invalid_initial_token(t) = isa(t, CharSymbol) && t in invalid
end

const is_closing_token = let closing = Set([',', ')', ']', '}', ';', SYM_ELSE, SYM_ELSEIF, SYM_CATCH, SYM_FINALLY])
    is_closing_token(ps::ParseState, t) =
        ((t === SYM_END && !ps.end_symbol) || Lexer.eof(t) || (isa(t, CharSymbol) && t in closing))
end

is_dict_literal(ex::Expr) = ex.head === :(=>) && length(ex.args) == 2
is_dict_literal(ex) = false

is_parameter(ex::Expr) = ex.head === :parameters && length(ex.args) == 1
is_parameter(ex) = false

function parse_chain(ps::ParseState, ts::TokenStream, down::Function, op)
    chain = Any[down(ps, ts)]
    while true
        t = peek_token(ps, ts)
        t !== op && return chain
        take_token(ts)
        if (ps.space_sensitive && ts.isspace &&
            (isa(t, Symbol) && t in Lexer.unary_and_binary_ops) &&
            Lexer.peekchar(ts) != ' ')
            # here we have "x -y"
            put_back!(ts, t)
            return chain
        end
        push!(chain, down(ps, ts))
    end
end

# parse left to right chains of certain binary operator
# ex. a + b + c => Expr(:call, :+, a, b, c)
function parse_with_chains{T}(ps::ParseState, ts::TokenStream, down::Function, ops::Set{T}, chain_op)
    ex = down(ps, ts)
    while true
        t = peek_token(ps, ts)
        !(t in ops) && return ex
        take_token(ts)
        if (ps.space_sensitive && ts.isspace && (t in Lexer.unary_and_binary_ops) && Lexer.peekchar(ts) != ' ')
            # here we have "x -y"
            put_back!(ts, t)
            return ex
        elseif t === chain_op
            ex = Expr(:call, t, ex); append!(ex.args, parse_chain(ps, ts, down, t))
        else
            ex = Expr(:call, t, ex, down(ps, ts))
        end
    end
end

function parse_LtoR{T}(ps::ParseState, ts::TokenStream, down::Function, ops::Set{T}, ex=down(ps, ts))
    while true
        t  = peek_token(ps, ts)
        !(t in ops) && return ex
        take_token(ts)
        if Lexer.is_syntactic_op(t) || t === :(in) || t === :(::)
            ex = Expr(t, ex, down(ps, ts))
        else
            ex = Expr(:call, t, ex, down(ps, ts))
        end
        t = peek_token(ps, ts)
    end
end

function parse_RtoL{T}(ps::ParseState, ts::TokenStream, down::Function, ops::Set{T}, ex=down(ps, ts))
    while true
        t  = peek_token(ps, ts)
        !(t in ops) && return ex
        take_token(ts)
        if (ps.space_sensitive && ts.isspace &&
            (isa(t, Symbol) && t in Lexer.unary_and_binary_ops) && Lexer.peekchar(ts) !== ' ')
            put_back!(ts, t)
            return ex
        elseif Lexer.is_syntactic_op(t)
            return Expr(t, ex, parse_RtoL(ps, ts, down, ops))
        elseif t === :(~)
            args = parse_chain(ps, ts, down, :(~))
            nt   = peek_token(ps, ts)
            if isa(nt, CharSymbol) && nt in ops
                ex = Expr(:macrocall, symbol("@~"), ex)
                for i=1:length(args)-1
                    push!(ex.args, args[i])
                end
                push!(ex.args, parse_RtoL(ps, ts, down, ops, args[end]))
                return ex
            else
                ex = Expr(:macrocall, symbol("@~"), ex); append!(ex.args, args)
                return ex
            end
        else
            return Expr(:call, t, ex, parse_RtoL(ps, ts, down, ops))
        end
    end
end

function parse_cond(ps::ParseState, ts::TokenStream)
    ex = parse_or(ps, ts)
    if peek_token(ps, ts) === :(?)
        take_token(ts)
        then = @without_range_colon ps begin
            parse_eqs(ps, ts)
        end
        take_token(ts) === :(:) || throw(ParseError("colon expected in \"?\" expression"))
        return Expr(:if, ex, then, parse_eqs(ps, ts))
    end
    return ex
end


function parse_Nary{T1, T2}(ps::ParseState, ts::TokenStream, down::Function, ops::Set{T1},
                            head::Symbol, closers::Set{T2}, allow_empty::Bool)
    t = require_token(ps, ts)
    is_invalid_initial_token(t) && throw(ParseError("unexpected \"$t\""))
    # empty block
    if isa(t, CharSymbol) && t in closers
        return Expr(head)
    end
    local args::Vector{Any}
    # in allow empty mode, skip leading runs of operator
    if allow_empty && isa(t, CharSymbol) && t in ops
        args = Any[]
    elseif '\n' in ops
        # line-number must happend before (down s)
        loc  = line_number_node(ts)
        args = Any[loc, down(ps, ts)]
    else
        args = Any[down(ps, ts)]
    end
    isfirst = true
    t = peek_token(ps, ts)
    while true
        if !(t in ops)
            if !(Lexer.eof(t) || t === '\n' || ',' in ops || t in closers)
                throw(ParseError("extra token \"$t\" after end of expression"))
            end
            if isempty(args) || length(args) >= 2 || !isfirst
                # [] => Expr(:head)
                # [ex1, ex2] => Expr(head, ex1, ex2)
                # (ex1) if operator appeared => Expr(head,ex1) (handles "x;")
                ex = Expr(head); ex.args = args
                return ex
            else
                # [ex1] => ex1
                return args[1]
            end
        end
        isfirst = false
        take_token(ts)
        # allow input to end with the operator, as in a;b;
        nt = peek_token(ps, ts)
        if Lexer.eof(nt) || (isa(nt, CharSymbol) && nt in closers) ||
           (allow_empty && isa(nt, CharSymbol) && nt in ops) ||
           (length(ops) == 1 && ',' in ops && nt === :(=))
           t = nt
           continue
        elseif '\n' in ops
            push!(args, line_number_node(ts))
            push!(args, down(ps, ts))
            t = peek_token(ps, ts)
        else
            push!(args, down(ps, ts))
            t = peek_token(ps, ts)
        end
    end
end

# the principal non-terminals follow, in increasing precedence order
const BLOCK_OPS = Set(['\n', ';'])
const BLOCK_CLOSERS = Set([SYM_END, SYM_ELSE, SYM_ELSEIF, SYM_CATCH, SYM_FINALLY])
function parse_block(ps::ParseState, ts::TokenStream)
    parse_Nary(ps, ts, parse_eq, BLOCK_OPS, :block, BLOCK_CLOSERS, true)
end

# for sequenced eval inside expressions, e.g. (a;b, c;d)
const WEXPR_OPS = Set([';'])
const WEXPR_CLOSERS = Set([',', ')'])
function parse_stmts_within_expr(ps::ParseState, ts::TokenStream)
    parse_Nary(ps, ts, parse_eqs, WEXPR_OPS, :block, WEXPR_CLOSERS, true)
end

#; at the top level produces a sequence of top level expressions
const NL_CLOSER = Set(['\n'])
function parse_stmts(ps::ParseState, ts::TokenStream)
    ex = parse_Nary(ps, ts, parse_eq, WEXPR_OPS, :toplevel, NL_CLOSER, true)
    # check for unparsed junk after an expression
    t = peek_token(ps, ts)
    if !(Lexer.eof(t) || t === '\n')
        throw(ParseError("extra token \"$t\" after end of expression"))
    end
    return ex
end

const EQ_OPS = Lexer.precedent_ops(1)
function parse_eq(ps::ParseState, ts::TokenStream)
    lno = curline(ts)
    ex  = parse_RtoL(ps, ts, parse_comma, EQ_OPS)
    return short_form_function_loc(ex, lno, filename(ts))
end

# parse-eqs is used where commas are special for example in an argument list
parse_eqs(ps::ParseState, ts::TokenStream)   = parse_RtoL(ps, ts, parse_cond, EQ_OPS)

# parse-comma is neeed for commas outside parens, for example a = b, c
const EMPTY_SET = Set()
const COMMA_OPS = Set([','])
parse_comma(ps::ParseState, ts::TokenStream) = parse_Nary(ps, ts, parse_cond, COMMA_OPS, :tuple, EMPTY_SET, false)

const OR_OPS = Lexer.precedent_ops(3)
parse_or(ps::ParseState, ts::TokenStream)    = parse_LtoR(ps, ts, parse_and, OR_OPS)

const AND_OPS = Lexer.precedent_ops(4)
parse_and(ps::ParseState, ts::TokenStream)   = parse_LtoR(ps, ts, parse_arrow, AND_OPS)

const ARROW_OPS = Lexer.precedent_ops(5)
parse_arrow(ps::ParseState, ts::TokenStream) = parse_RtoL(ps, ts, parse_ineq, ARROW_OPS)

const INEQ_OPS = Lexer.precedent_ops(6)
parse_ineq(ps::ParseState, ts::TokenStream)  = parse_comparison(ps, ts, INEQ_OPS)

const PIPES_OPS = Lexer.precedent_ops(7)
parse_pipes(ps::ParseState, ts::TokenStream) = parse_LtoR(ps, ts, parse_range, PIPES_OPS)

const IN_OPS = Set([:(in)])
parse_in(ps::ParseState, ts::TokenStream)    = parse_LtoR(ps, ts, parse_pipes, IN_OPS)

const EXPR_OPS = Lexer.precedent_ops(9)
parse_expr(ps::ParseState, ts::TokenStream)  = parse_with_chains(ps, ts, parse_shift, EXPR_OPS, :(+))

const SHIFT_OPS = Lexer.precedent_ops(10)
parse_shift(ps::ParseState, ts::TokenStream) = parse_LtoR(ps, ts, parse_term, SHIFT_OPS)

const TERM_OPS = Lexer.precedent_ops(11)
parse_term(ps::ParseState, ts::TokenStream)  = parse_with_chains(ps, ts, parse_rational, TERM_OPS, :(*))

const RAT_OPS = Lexer.precedent_ops(12)
parse_rational(ps::ParseState, ts::TokenStream) = parse_LtoR(ps, ts, parse_unary, RAT_OPS)

function parse_comparison(ps::ParseState, ts::TokenStream, ops)
    ex = parse_in(ps, ts)
    isfirst = true
    while true
        t = peek_token(ps, ts)
        !(t in ops) && return ex
        take_token(ts)
        if isfirst
            isfirst = false
            ex = Expr(:comparison, ex, t, parse_range(ps, ts))
        else
            push!(ex.args, t)
            push!(ex.args, parse_range(ps, ts))
        end
    end
end

is_large_number(n::BigInt) = true
is_large_number(n::Number) = false

const is_juxtaposed = let invalid_chars = Set{Char}(['(', '[', '{'])
    is_juxtaposed(ps::ParseState, ex, t) = begin
        return !(Lexer.is_operator(t)) &&
               !(Lexer.is_operator(ex)) &&
               !(t in Lexer.reserved_words) &&
               !(is_closing_token(ps, t)) &&
               !(Lexer.isnewline(t)) &&
               !(isa(ex, Expr) && ex.head === :(...)) &&
               ((isa(ex, Number) && !isa(ex, Char)) || !in(t, invalid_chars))
    end
end

#= This handles forms such as 2x => Expr(:call, :*, 2, :x) =#
function parse_juxtaposed(ps::ParseState, ts::TokenStream, ex)
    # numeric literal juxtaposition is a unary operator
    if is_juxtaposed(ps, ex, peek_token(ps, ts)) && !ts.isspace
        return Expr(:call, :(*), ex, parse_unary(ps, ts))
    end
    return ex
end

function parse_range(ps::ParseState, ts::TokenStream)
    ex = parse_expr(ps, ts)
    isfirst = true
    while true
        t = peek_token(ps, ts)
        if isfirst && t === :(..)
           take_token(ts)
           return Expr(:call, t, ex, parse_expr(ps, ts))
        end
        if ps.range_colon_enabled && t === :(:)
            take_token(ts)
            if ps.space_sensitive && ts.isspace
                peek_token(ps, ts)
                if !ts.isspace
                    # "a :b" in space sensitive mode
                    put_back!(ts, :(:))
                    return ex
                end
            end
            if is_closing_token(ps, peek_token(ps, ts))
                # handles :(>:) case
                if isa(ex, Symbol) && Lexer.is_operator(ex)
                    op = symbol(string(ex, t))
                    Lexer.is_operator(op) && return op
                end
                throw(ParseError("missing last argument in \"$(ex):\"range expression"))
            end
            if Lexer.isnewline(peek_token(ps, ts))
                throw(ParseError("line break in \":\" expression"))
            end
            arg = parse_expr(ps, ts)
            if !ts.isspace && (arg === :(<) || arg === :(>))
                throw(ParseError("\":$argument\" found instead of \"$argument:\""))
            end
            if isfirst
                ex = Expr(t, ex, arg)
                isfirst = false
            else
                push!(ex.args, arg)
                isfirst = true
            end
            continue
        elseif t === :(...)
            take_token(ts)
            return Expr(:(...), ex)
        else
            return ex
        end
    end
end

function parse_decl(ps::ParseState, ts::TokenStream)
    ex = parse_call(ps, ts)
    while true
        nt = peek_token(ps, ts)
        # type assertion => x::Int
        if nt === :(::)
            take_token(ts)
            ex = Expr(:(::), ex, parse_call(ps, ts))
            continue
        end
        # anonymous function => (x) -> x + 1
        if nt === :(->)
            take_token(ts)
            # -> is unusual it binds tightly on the left and loosely on the right
            lno = line_number_filename_node(ts)
            return Expr(:(->), ex, Expr(:block, lno, parse_eqs(ps, ts)))
        end
        return ex
    end
end

# handle ^ and .^
function parse_factorh(ps::ParseState, ts::TokenStream, down::Function, ops)
    ex = down(ps, ts)
    nt = peek_token(ps, ts)
    !(nt in ops) && return ex
    take_token(ts)
    pf = parse_factorh(ps, ts, parse_unary, ops)
    return Expr(:call, nt, ex, pf)
end

function negate(n)
    if isa(n, Number)
        if isa(n, Int64) && n == -9223372036854775808
            # promote to Int128
            return 9223372036854775808
        end
        if isa(n, Int128) && n == -170141183460469231731687303715884105728
            # promote to BigInt
            return BigInt("170141183460469231731687303715884105728")
        end
        return -n
    elseif isa(n, Expr)
        return n.head === :(-) && length(n.args) == 1 ? n.args[1] : Expr(:-, n)
    end
    throw(ArgumentError("negate argument is not a Number or Expr"))
end

# -2^3 is parsed as -(2^3) so call parse-decl for the first arg,
# and parse unary from then on (handles 2^-3)
const FAC_OPS = Lexer.precedent_ops(13)
parse_factor(ps::ParseState, ts::TokenStream) = parse_factorh(ps, ts, parse_decl, FAC_OPS)

function parse_unary(ps::ParseState, ts::TokenStream)
    t = require_token(ps, ts)
    is_closing_token(ps, t) && throw(ParseError("unexpected $t"))
    if !(isa(t, Symbol) && t in Lexer.unary_ops)
        pf = parse_factor(ps, ts)
        return parse_juxtaposed(ps, ts, pf)
    end
    op = take_token(ts)
    nc = Lexer.peekchar(ts)
    if (op === :(-) || op === :(+)) && (isdigit(nc) || nc === '.')
        neg = op === :(-)
        leadingdot = nc === '.'
        leadingdot && Lexer.readchar(ts)
        n   = Lexer.read_number(ts, leadingdot, neg)
        num = parse_juxtaposed(ps, ts, n)
        nt  = peek_token(ps, ts)
        if nt === :(^) || nt === :(.^)
            # -2^x parsed as (- (^ 2 x))
            put_back!(ts, neg ? negate(num) : num)
            return Expr(:call, op, parse_factor(ps, ts))
        end
        return num
    end
    nt = peek_token(ps, ts)
    if is_closing_token(ps, nt) || Lexer.isnewline(nt)
        # return operator by itself, as in (+)
        return op
    elseif nt === '{'
        # this case is +{T}(x::T)
        put_back!(ts, op)
        return parse_factor(ps, ts)
    else
        arg = parse_unary(ps, ts)
        if isa(arg, Expr) && arg.head === :tuple
            ex = Expr(:call, op); append!(ex.args, arg.args)
            return ex
        end
        return Expr(:call, op, arg)
    end
end

function subtype_syntax(ex)
    if isa(ex, Expr) && ex.head === :comparison && length(ex.args) == 3 && ex.args[2] === :(<:)
        return Expr(:(<:), ex.args[1], ex.args[3])
    end
    return ex
end

function parse_unary_prefix(ps::ParseState, ts::TokenStream)
    op = peek_token(ps, ts)
    if isa(op, Symbol) && Lexer.is_syntactic_unary_op(op)
        take_token(ts)
        if is_closing_token(ps, peek_token(ps, ts))
            return op
        elseif op === :(&) || op === :(::)
            return Expr(op, parse_call(ps, ts))
        else
            return Expr(op, parse_atom(ps, ts))
        end
    end
    return parse_atom(ps, ts)
end

# parse function all, indexing, dot, and transpose expressions
# also handles looking for reserved words
function parse_call(ps::ParseState, ts::TokenStream)
    ex = parse_unary_prefix(ps, ts)
    if isa(ex, Symbol) && ex in Lexer.reserved_words
        return parse_resword(ps, ts, ex)
    end
    return parse_call_chain(ps, ts, ex, false)
end

function separate(f::Function, collection)
    tcoll, fcoll = Any[], Any[]
    for c in collection
        f(c) ? push!(tcoll, c) : push!(fcoll, c)
    end
    return (tcoll, fcoll)
end

const BEGIN_CHARS = Set(['(', '[','{', '"'])

function parse_call_chain(ps::ParseState, ts::TokenStream, ex, one_call::Bool)
    while true
        t = peek_token(ps, ts)
        if (ps.space_sensitive && ts.isspace &&
           (t === SYM_SQUOTE || t in BEGIN_CHARS) ||
           (isa(ex, Number) && t === '('))
            return ex
        end
        if t === '('
            take_token(ts)
            arglist = parse_arglist(ps, ts, ')')
            params, args = separate(is_parameter, arglist)
            if peek_token(ps, ts) === SYM_DO
                take_token(ts)
                ex = Expr(:call, ex)
                append!(ex.args, params)
                push!(ex.args, parse_do(ps, ts))
                append!(ex.args, args)
            else
                ex = Expr(:call, ex)
                append!(ex.args, arglist)
            end
            one_call && return ex
            continue

        elseif t === '['
            take_token(ts)
            # ref is syntax so can distinguish a[i] = x from ref(a, i) = x
            al = @with_end_symbol ps begin
                parse_cat(ps, ts, ']', is_dict_literal(ex))
            end
            if (al.head === :cell1d || al.head === :vcat) && isempty(al.args)
                ex = is_dict_literal(ex) ? Expr(:typed_dict, ex) : Expr(:ref, ex)
                continue
            end
            if al.head === :dict
                ex = Expr(:typed_dict, ex); append!(ex.args, al.args)
            elseif al.head === :hcat
                ex = Expr(:typed_hcat, ex); append!(ex.args, al.args)
            elseif al.head === :vcat
                ex = Expr(:ref, ex)
                for arg in al.args
                    if isa(arg, Expr) && arg.head === :row
                        ex.head = :typed_vcat
                    end
                    push!(ex.args, arg)
                end
            elseif al.head === :comprehension
                ex = Expr(:typed_comprehension, ex); append!(ex.args, al.args)
            elseif al.head === :dict_comprehension
                ex = Expr(:typed_dict_comprehension, ex); append!(ex.args, al.args)
            else
                throw(ParseError("unknown parse-cat result (internal error)"))
            end
            continue

        elseif t === :(.)
            take_token(ts)
            nt = peek_token(ps, ts)
            if nt === '('
                ex = Expr(:(.), ex, parse_atom(ps, ts))
            elseif nt === :($)
                dollar_ex = parse_unary(ps, ts)
                call_ex   = Expr(:call, TopNode(:Expr), Expr(:quote, :quote), dollar_ex.args[1])
                ex = Expr(:(.), ex, Expr(:($), call_ex))
            else
                name = parse_atom(ps, ts)
                if isa(name, Expr) && name.head === :macrocall
                    ex = Expr(:macrocall, Expr(:(.), ex, Expr(:quote, name.args[1])))
                    append!(ex.args, name.args[2:end])
                else
                    ex = Expr(:(.), ex, Expr(:quote, name))
                end
            end
            continue

        elseif t === :(.') || t === SYM_SQUOTE
            take_token(ts)
            ex = Expr(t, ex)
            continue

        elseif t === '{'
            take_token(ts)
            args = map(subtype_syntax, parse_arglist(ps, ts, '}'))
            # ::Type{T}
            if isa(ex, Expr) && ex.head == :(::)
                ty = Expr(:curly, ex.args[1]); append!(ty.args, args)
                ex = Expr(:(::), ty)
            else
                ex = Expr(:curly, ex); append!(ex.args, args)
            end
            continue

        elseif t === '"'
            if isa(ex, Symbol) && !Lexer.is_operator(ex) && !ts.isspace
                # custom prefexed string literals x"s" => @x_str "s"
                take_token(ts)
                str = parse_string_literal(ps, ts, true)
                nt  = peek_token(ps, ts)
                suffix  = triplequote_string_literal(str) ? "_mstr" : "_str"
                macname = symbol(string('@', ex, suffix))
                macstr  = str.args[1]
                if isa(nt, Symbol) && !Lexer.is_operator(nt) && !ts.isspace
                    # string literal suffix "s"x
                    ex = Expr(:macrocall, macname, macstr, string(take_token(ts)))
                else
                    ex = Expr(:macrocall, macname, macstr)
                end
                continue
            end
            return ex
        end
        return ex
    end
end

const expect_end_current_line = 0

function expect_end(ps::ParseState, ts::TokenStream, word::Symbol)
    t = peek_token(ps, ts)
    if t === SYM_END
        take_token(ts)
    elseif Lexer.eof(t)
        err_msg = "incomplete: \"$word\" at \"$(filename(ts))\" : {expected} requires end"
        error(err_msg)
    else
        err_msg = "incomplete: \"$word\" at \"$(filename(ts))\" : {expected} \"end\", got \"$t\""
        error(err_msg)
    end
end

parse_subtype_spec(ps::ParseState, ts::TokenStream) = subtype_syntax(parse_ineq(ps, ts))


#TODO: remove this after the 0.4 dev period
@eval function tryexpr(tryb, catchv, catchb, finalb)
    if finalb == nothing
        return catchb != nothing ? Expr(:try, tryb, catchv, catchb) :
                                   $(VERSION > v"0.4.0-dev" ?
                                        :(Expr(:try, tryb, false, Expr(:block))) :
                                        :(Expr(:try, tryb, false, false)))
    else
        return catchb != nothing ? Expr(:try, tryb, catchv, catchb, finalb) :
                                   $(VERSION > v"0.4.0-dev" ?
                                        :(Expr(:try, tryb, false, false, finalb)) :
                                        :(Expr(:try, tryb, false, false, finalb)))
    end
end

# parse expressions or blocks introduced by syntatic reserved words
function parse_resword(ps::ParseState, ts::TokenStream, word::Symbol)
    expect_end_current_line = curline(ts)
    @with_normal_ops ps begin
        @without_whitespace_newline ps begin
            if word === :quote || word === :begin
                Lexer.skipws_and_comments(ts)
                loc = line_number_filename_node(ts)
                blk = parse_block(ps, ts)
                expect_end(ps, ts, word)
                local ex::Expr
                if !isempty(blk.args) &&
                    ((isa(blk.args[1], Expr) && blk.args[1].head === :line) || (isa(blk.args[1], LineNumberNode)))
                    ex = Expr(:block, loc)
                    for i in 2:length(blk.args)
                        push!(ex.args, blk.args[i])
                    end
                else
                    ex = blk
                end
                return word === :quote ? Expr(:quote, ex) : ex

            elseif word === :while
                ex = Expr(:while, parse_cond(ps, ts), parse_block(ps, ts))
                expect_end(ps, ts, word)
                return ex

            elseif word === :for
                ranges  = parse_comma_sep_iters(ps, ts)
                nranges = length(ranges)
                body = parse_block(ps, ts)
                expect_end(ps, ts, word)
                if nranges == 1
                    return Expr(:for, ranges[1], body)
                else
                    blk = Expr(:block); blk.args = ranges
                    return Expr(:for, blk, body)
                end

            elseif word === :if
                test = parse_cond(ps, ts)
                t    = require_token(ps, ts)
                then = (t === SYM_ELSE || t === SYM_ELSEIF) ? Expr(:block) :
                                                              parse_block(ps, ts)
                nxt = require_token(ps, ts)
                take_token(ts)
                if nxt === SYM_END
                    return Expr(:if, test, then)
                elseif nxt === SYM_ELSEIF
                    if Lexer.isnewline(peek_token(ps, ts))
                        throw(ParseError("missing condition in elseif at {filename} : {line}"))
                    end
                    blk = Expr(:block, line_number_node(ts), parse_resword(ps, ts, :if))
                    return Expr(:if, test, then, blk)
                elseif nxt === SYM_ELSE
                    if peek_token(ps, ts) === :if
                        throw(ParseError("use elseif instead of else if"))
                    end
                    blk = parse_block(ps, ts)
                    ex = Expr(:if, test, then, blk)
                    expect_end(ps, ts, word)
                    return ex
                else
                    throw(ParseError("unexpected next token $nxt in if"))
                end

            elseif word === :let
                nt = peek_token(ps, ts)
                binds = Lexer.isnewline(nt) || nt === ';' ? Any[] : parse_comma_sep_assigns(ps, ts)
                nt = peek_token(ps, ts)
                if !(Lexer.eof(nt) || (isa(nt, CharSymbol) && (nt === '\n' || nt ===  ';' || nt === SYM_END)))
                    throw(ParseError("let variables should end in \";\" or newline"))
                end
                ex = parse_block(ps, ts)
                expect_end(ps, ts, word)
                ex = Expr(:let, ex); append!(ex.args, binds)
                return ex

            elseif word === :global || word === :local
                lno = curline(ts)
                isconst = peek_token(ps, ts) === :const ? (take_token(ts); true) : false
                args = Any[]
                for aex in parse_comma_sep_assigns(ps, ts)
                    push!(args, short_form_function_loc(aex, lno, filename(ts)))
                end
                if isconst
                    ex = Expr(word); ex.args = args
                    return Expr(:const, ex)
                else
                    ex = Expr(word); ex.args = args
                    return ex
                end

            elseif word === :function || word === :macro || word === :stagedfunction
                paren = require_token(ps, ts) === '('
                sig   = parse_call(ps, ts)
                local def::Expr
                if isa(sig, Symbol) ||
                    (isa(sig, Expr) && sig.head === :(::) && isa(sig.args[1], Symbol))
                   if paren
                       # in function(x) the (x) is a tuple
                       def = Expr(:tuple, sig)
                    else
                       # function foo => syntax error
                       throw(ParseError("expected \"(\" in $word definition"))
                    end
                else
                    if (isa(sig, Expr) && (sig.head === :call || sig.head === :tuple))
                        def = sig
                    else
                        throw(ParseError("expected \"(\" in $word definition"))
                    end
                end
                peek_token(ps, ts) !== SYM_END && Lexer.skipws_and_comments(ts)
                loc  = line_number_filename_node(ts)
                body = parse_block(ps, ts)
                expect_end(ps, ts, word)
                add_filename_to_block!(body, loc)
                return Expr(word, def, body)

            elseif word === :abstract
                return Expr(:abstract, parse_subtype_spec(ps, ts))

            elseif word === :type || word === :immutable
                istype = word === :type
                # allow "immutable type"
                (!istype && peek_token(ps, ts) === :type) && take_token(ts)
                sig = parse_subtype_spec(ps, ts)
                blk = parse_block(ps, ts)
                ex  = Expr(:type, istype, sig, blk)
                expect_end(ps, ts, word)
                return ex

            elseif word === :bitstype
                stmnt = @space_sensitive ps begin
                    parse_cond(ps, ts)
                end
                return Expr(:bitstype, stmnt, parse_subtype_spec(ps, ts))

            elseif word === :typealias
                lhs = parse_call(ps, ts)
                if isa(lhs, Expr) && lhs.head === :call
                    # typealias X (...) is a tuple type alias, not call
                    return Expr(:typealias, lhs.args[1], Expr(:tuple, lhs.args[2:end]...))
                else
                    return Expr(:typealias, lhs, parse_arrow(ps, ts))
                end

            elseif word === :try
                t = require_token(ps, ts)
                tryb = t === SYM_CATCH || t === SYM_FINALLY ? Expr(:block) : parse_block(ps, ts)
                t = require_token(ps, ts)
                catchb = nothing
                catchv = false
                finalb = nothing
                while true
                    take_token(ts)
                    if t === SYM_END
                        return tryexpr(tryb, catchv, catchb, finalb)
                    end
                    if t === SYM_CATCH && catchb == nothing
                        nb = false # do we delineate a new block after a catch token (with ; or \n)?
                        nt = peek_token(ps, ts)
                        if Lexer.isnewline(nt) || nt === ';'
                            nb = true
                            nt === ';' && take_token(ts)
                        end
                        t = require_token(ps, ts)
                        if t === SYM_END || t === SYM_FINALLY
                            catchb = Expr(:block)
                            catchv = false
                            continue
                        else
                            var   = parse_eqs(ps, ts)
                            isvar = nb == false && isa(var, Symbol)
                            catch_block = require_token(ps, ts) === SYM_FINALLY ? Expr(:block) :
                                                                                  parse_block(ps, ts)
                            t = require_token(ps, ts)
                            if isvar
                                catchb = catch_block
                            else
                                exb = Expr(:block, var)
                                append!(exb.args, catch_block.args);
                                catchb = exb
                            end
                            catchv = isvar ? var : false
                            continue
                        end
                    elseif t === SYM_FINALLY && finalb == nothing
                        finalb = require_token(ps, ts) === SYM_CATCH ? Expr(:block) :
                                                                       parse_block(ps, ts)
                        t = require_token(ps, ts)
                        continue
                    else
                        throw(ParseError("unexpected \"$t\""))
                    end
                end

            elseif word === :return
                t  = peek_token(ps, ts)
                return Lexer.isnewline(t) || is_closing_token(ps, t) ? Expr(:return, nothing) :
                                                                       Expr(:return, parse_eq(ps, ts))
            elseif word === :break || word === :continue
                return Expr(word)

            elseif word === :const
                assgn = parse_eq(ps, ts)
                if !(isa(assgn, Expr) && (assgn.head === :(=) ||
                                          assgn.head === :global ||
                                          assgn.head === :local))
                    throw(ParseError("expected assignment after \"const\""))
                end
                return Expr(:const, assgn)

            elseif word === :module || word === :baremodule
                isbare = word === :baremodule
                name = parse_atom(ps, ts)
                body = parse_block(ps, ts)
                expect_end(ps, ts, word)
                if !isbare
                    # add definitions for module_local eval
                    block = Expr(:block)
                    x = name === :x ? :y : :x
                    push!(block.args,
                        Expr(:(=), Expr(:call, :eval, x),
                                   Expr(:call, Expr(:(.), TopNode(:Core),
                                               Expr(:quote, :eval)), name, x)))
                    push!(block.args,
                        Expr(:(=), Expr(:call, :eval, :m, :x),
                                   Expr(:call, Expr(:(.), TopNode(:Core),
                                               Expr(:quote, :eval)), :m, :x)))
                    append!(block.args, body.args)
                    body = block
                end
                return Expr(:module, !isbare, name, body)

            elseif word === :export
                exports = map(macrocall_to_atsym, parse_comma_sep(ps, ts, parse_atom))
                !all(x -> isa(x, Symbol), exports) && throw(ParseError("invalid \"export\" statement"))
                ex = Expr(:export); ex.args = exports
                return ex

            elseif word === :import || word === :using || word === :importall
                imports = parse_imports(ps, ts, word)
                length(imports) == 1 && return imports[1]
                ex = Expr(:toplevel); ex.args = imports
                return ex

            elseif word === :ccall
                peek_token(ps, ts) != '(' && throw(ParseError("invalid \"ccall\" syntax"))
                take_token(ts)
                al = parse_arglist(ps, ts, ')')
                if length(al) > 1
                    al1, al2 = al[1], al[2]
                    if al2 === :cdecl || al2 === :stdcall || al2 == :fastcall || al2 == :thiscall
                        # place calling convention at end of arglist
                        ex = Expr(:ccall, al1)
                        for i = 3:length(al)
                            push!(ex.args, al[i])
                        end
                        push!(ex.args, Expr(al2))
                        return ex
                    end
                end
                ex = Expr(:ccall); ex.args = al
                return ex

            elseif word === :do
                throw(ParseError("invalid \"do\" syntax"))

            else
                throw(ParseError("unhandled reserved word $word"))
            end
        end
    end
end

function add_filename_to_block!(body::Expr, loc::Expr)
    if !isempty(body.args)
        if isa(body.args[1], Expr) && body.args[1].head === :line
            body.args[1] = loc
        elseif isa(body.args[1], LineNumberNode)
            body.args[1] = loc
        end
    end
    return body
end

function parse_do(ps::ParseState, ts::TokenStream)
    expect_end_current_line = curline(ts)
    @without_whitespace_newline ps begin
        doargs = Lexer.isnewline(peek_token(ps, ts)) ? Any[] : parse_comma_sep(ps, ts, parse_range)
        loc = line_number_filename_node(ts)
        blk = parse_block(ps, ts)
        add_filename_to_block!(blk, loc)
        expect_end(ps, ts, :do)
        return Expr(:(->), Expr(:tuple, doargs...), blk)
    end
end

macrocall_to_atsym(ex) = isa(ex, Expr) && ex.head === :macrocall ? ex.args[1] : ex

function parse_imports(ps::ParseState, ts::TokenStream, word::Symbol)
    frst = Any[parse_import(ps, ts, word)]
    nt   = peek_token(ps, ts)
    from = nt === :(:) && !ts.isspace
    done = false
    if from || nt === ','
        take_token(ts)
        done = false
    elseif nt in ('\n', ';')
        done = true
    elseif Lexer.eof(nt)
        done = true
    else
        done = false
    end
    rest = done? Any[] : parse_comma_sep(ps, ts, (ps, ts) -> parse_import(ps, ts, word))
    if from
        module_syms = frst[1].args
        imports = Expr[]
        for expr in rest
            ex = Expr(expr.head, module_syms..., expr.args...)
            push!(imports, ex)
        end
        return imports
    end
    return append!(frst, rest)
end

const sym_1dot  = symbol(".")
const sym_2dots = symbol("..")
const sym_3dots = symbol("...")
const sym_4dots = symbol("....")

function parse_import_dots(ps::ParseState, ts::TokenStream)
    l = Any[]
    t = peek_token(ps, ts)
    while true
        if t === sym_1dot
            take_token(ts)
            push!(l, :(.))
            t = peek_token(ps, ts)
            continue
        elseif t === sym_2dots
            take_token(ts)
            append!(l, [:(.), :(.)])
            t = peek_token(ps, ts)
            continue
        elseif t === sym_3dots
            take_token(ts)
            append!(l, [:(.), :(.), :(.)])
            t = peek_token(ps, ts)
            continue
        elseif t === sym_4dots
            take_token(ts)
            append!(l, [:(.), :(.), :(.), :(.)])
            t = peek_token(ps, ts)
            continue
        end
        return push!(l, macrocall_to_atsym(parse_atom(ps, ts)))
    end
end

function parse_import(ps::ParseState, ts::TokenStream, word::Symbol)
    path = parse_import_dots(ps, ts)
    while true
        # this handles cases such as Base.* where .* is a valid operator token
        nc = Lexer.peekchar(ts)
        if nc === '.'
            Lexer.takechar(ts)
            push!(path, macrocall_to_atsym(parse_atom(ps, ts)))
            continue
        end
        nt = peek_token(ps, ts)
        if (Lexer.eof(nt) ||
            (isa(nt, CharSymbol) && (nt === '\n' || nt === ';' || nt === ',' || nt === :(:))))
            ex = Expr(word); ex.args = path
            return ex
        else
            throw(ParseError("invalid \"$word\" statement"))
        end
    end
end

function parse_comma_sep(ps::ParseState, ts::TokenStream, what::Function)
    exprs = Any[]
    while true
        r = what(ps, ts)
        if peek_token(ps, ts) === ','
            take_token(ts)
            push!(exprs, r)
            continue
        end
        push!(exprs, r)
        return exprs
    end
end

parse_comma_sep_assigns(ps::ParseState, ts::TokenStream) = parse_comma_sep(ps, ts, parse_eqs)

# as above, but allows both "i=r" and "i in r"
# return a list of range expressions
function parse_comma_sep_iters(ps::ParseState, ts::TokenStream)
    ranges = Any[]
    while true
        r = parse_eqs(ps, ts)
        if r === :(:)
        elseif isa(r, Expr) && r.head === :(=)
        elseif isa(r, Expr) && r.head === :in
            tmp = r; r = Expr(:(=)); r.args = tmp.args
        else
            throw(ParseError("invalid iteration spec"))
        end
        if peek_token(ps, ts) === ','
            take_token(ts)
            push!(ranges, r)
            continue
        end
        push!(ranges, r)
        return ranges
    end
end

function parse_space_separated_exprs(ps::ParseState, ts::TokenStream)
    @space_sensitive ps begin
        exprs = Any[]
        while true
            nt = peek_token(ps, ts)
            if is_closing_token(ps, nt) ||
               Lexer.isnewline(nt) ||
               (ps.inside_vector && nt === :for)
                return exprs
            end
            ex = parse_eq(ps, ts)
            if Lexer.isnewline(peek_token(ps, ts))
                push!(exprs, ex)
                return exprs
            end
            push!(exprs, ex)
        end
    end
end

function to_kws(lst)
    n = length(lst)
    kwargs = Array(Any, n)
    for i = 1:n
        ex = lst[i]
        if isa(ex, Expr) && ex.head === :(=) && length(ex.args) == 2
            nex = Expr(:kw); nex.args = ex.args
            kwargs[i] = nex
        else
            kwargs[i] = ex
        end
    end
    return kwargs
end

# handle function call argument list, or any comma-delimited list
# * an extra comma at the end is allowed
# * expressions after a ; are enclosed in (parameters ....)
# * an expression followed by ... becomes (.... x)
function _parse_arglist(ps::ParseState, ts::TokenStream, closer)
    lst = Any[]
    while true
        t = require_token(ps, ts)
        if t === closer
            take_token(ts)
            # x=y inside a function call is a keyword argument
            return closer === ')' ? to_kws(lst) : lst
        elseif t === ';'
            take_token(ts)
            # allow f(a, b; )
            peek_token(ps, ts) === closer && continue
            params = parse_arglist(ps, ts, closer)
            lst = closer === ')' ? to_kws(lst) : lst
            return unshift!(lst, Expr(:parameters, params...))
        end
        nxt = parse_eqs(ps, ts)
        nt  = require_token(ps, ts)
        if nt === ','
            take_token(ts)
            push!(lst, nxt)
            continue
        elseif nt === ';'
            push!(lst, nxt)
            continue
        elseif nt === closer
            push!(lst, nxt)
            continue
        elseif nt in (']', '}')
            throw(ParseError("unexpected \"$nt\" in argument list"))
        else
            throw(ParseError("missing comma or \"$closer\" in argument list"))
        end
    end
end

function parse_arglist(ps::ParseState, ts::TokenStream, closer)
    @with_normal_ops ps begin
        @with_whitespace_newline ps begin
            return _parse_arglist(ps, ts, closer)
        end
    end
end

# parse [] concatenation exprs and {} cell exprs
function parse_vcat(ps::ParseState, ts::TokenStream, frst, closer)
    lst = Any[]
    nxt = frst
    while true
        t = require_token(ps, ts)
        if t === closer
            take_token(ts)
            ex = Expr(:vcat); ex.args = push!(lst, nxt)
            return ex
        end
        if t === ','
            take_token(ts)
            if require_token(ps, ts) === closer
                # allow ending with ,
                take_token(ts)
                ex = Expr(:vcat); ex.args = push!(lst, nxt)
                return ex
            end
            lst = push!(lst, nxt)
            nxt = parse_eqs(ps, ts)
            continue
        elseif t === ';'
            take_token(ts)
            peek_token(ps, ts) === closer && continue
            pex = Expr(:parameters); ex.args = parse_arglist(ps, ts, closer)
            unshift!(pex.args, lst)
            ex = Expr(:vcat); ex.args = reverse(push!(lst, next))
            return ex
        elseif t === ']' || t === '}'
            throw(ParseError("unexpected \"$t\" in array expression"))
        else
            throw(ParseError("missing separator in array expression"))
        end
    end
end


function parse_dict(ps::ParseState, ts::TokenStream, frst, closer)
    v = parse_vcat(ps, ts, frst, closer)
    local alldl::Bool
    for arg in v.args
        alldl = is_dict_literal(arg)
        alldl || break
    end
    if alldl
        ex = Expr(:dict); ex.args = v.args
        return ex
    else
        throw(ParseError("invalid dict literal"))
    end
end

function parse_comprehension(ps::ParseState, ts::TokenStream, frst, closer)
    itrs = parse_comma_sep_iters(ps, ts)
    t = require_token(ps, ts)
    t === closer ? take_token(ts) : throw(ParseError("expected $closer"))
    ex = Expr(:comprehension, frst); append!(ex.args, itrs)
    return ex
end

function parse_dict_comprehension(ps::ParseState, ts::TokenStream, frst, closer)
    c = parse_comprehension(ps, ts, frst, closer)
    if is_dict_literal(c.args[1])
        ex = Expr(:dict_comprehension); ex.args = c.args
        return ex
    else
        throw(ParseError("invalid dict comprehension"))
    end
end


function parse_matrix(ps::ParseState, ts::TokenStream, frst, closer)

    update_outer!(v, outer) = begin
        len = length(v)
        len == 0 && return outer
        len == 1 && return push!(outer, v[1])
        row = Expr(:row); row.args = v
        return push!(outer, row)
    end

    semicolon = peek_token(ps, ts) === ';'
    vec   = Any[frst]
    outer = Any[]
    while true
        t = peek_token(ps, ts) === '\n' ? '\n' : require_token(ps, ts)
        if t === closer
            take_token(ts)
            local ex::Expr
            if !isempty(outer)
                ex = Expr(:vcat); ex.args = update_outer!(vec, outer)
            elseif length(vec) <= 1
                # [x] => (vcat x)
                ex = Expr(:vcat); ex.args = vec
            else
                # [x y] => (hcat x y)
                ex = Expr(:hcat); ex.args = vec
            end
            return ex
        end
        if t === ';' || t === '\n'
            take_token(ts)
            outer = update_outer!(vec, outer)
            vec   = Any[]
            continue
        elseif t === ','
            throw(ParseError("unexpected comma in matrix expression"))
        elseif t === ']' || t === '}'
            throw(ParseError("unexpected \"$t\""))
        elseif t === :for
            if !semicolon && length(outer) == 1 && isempty(vec)
                take_token(ts)
                return parse_comprehension(ps, ts, outer[1], closer)
            else
                throw(ParseError("invalid comprehension syntax"))
            end
        else
            push!(vec, parse_eqs(ps, ts))
            continue
        end
    end
end

function peek_non_newline_token(ps::ParseState, ts::TokenStream)
    while true
        t = peek_token(ps, ts)
        if Lexer.isnewline(t)
            take_token(ts)
            continue
        end
        return t
    end
end

function parse_cat(ps::ParseState, ts::TokenStream, closer, isdict::Bool=false)
    @with_normal_ops ps begin
        @with_inside_vec ps begin
            if require_token(ps, ts) === closer
                take_token(ts)
                if closer === '}'
                    return Expr(:cell1d)
                elseif closer === ']'
                    return Expr(:vcat)
                else
                    throw(ParseError("unknown closer $closer"))
                end
            end
            frst = parse_eqs(ps, ts)
            if is_dict_literal(frst)
                nt = peek_non_newline_token(ps, ts)
                if nt === :for
                    take_token(ts)
                    return parse_dict_comprehension(ps, ts, frst, closer)
                else
                    return parse_dict(ps, ts, frst, closer)
                end
            end
            nt = peek_token(ps, ts)
            if nt === ','
                return parse_vcat(ps, ts, frst, closer)
            elseif nt === :for
                take_token(ts)
                return parse_comprehension(ps, ts, frst, closer)
            else
                return parse_matrix(ps, ts, frst, closer)
            end
        end
    end
end

function parse_tuple(ps::ParseState, ts::TokenStream, frst)
    args = Any[]
    nxt = frst
    while true
        t = require_token(ps, ts)
        if t === ')'
            take_token(ts)
            ex = Expr(:tuple); ex.args = push!(args, nxt)
            return ex
        end
        if t === ','
            take_token(ts)
            if require_token(ps, ts) === ')'
                # allow ending with ,
                take_token(ts)
                ex = Expr(:tuple); ex.args = push!(args, nxt)
                return ex
            end
            args = push!(args, nxt)
            nxt  = parse_eqs(ps, ts)
            continue
        elseif t === ';'
            throw(ParseError("unexpected semicolon in tuple"))
        elseif t === ']' || t === '}'
            throw(ParseError("unexpected \"$(peek_token(ps, ts))\" in tuple"))
        else
            throw(ParseError("missing separator in tuple"))
        end
    end
end

# TODO: these are unnecessary if base/client.jl didn't need to parse error string
function not_eof_1(c)
    Lexer.eof(c) && error("incomplete: invalid character literal")
    return c
end

function not_eof_2(c)
    Lexer.eof(c) && error("incomplete: invalid \"`\" syntax")
    return c
end

function not_eof_3(c)
    Lexer.eof(c) && error("incomplete: invalid string syntax")
    return c
end

#TODO; clean up eof handling
function parse_backquote(ps::ParseState, ts::TokenStream)
    buf = IOBuffer()
    c   = Lexer.readchar(ts)
    while true
        c === '`' && break
        if c === '\\'
            nc = Lexer.readchar(ts)
            if nc === '`'
                write(buf, nc)
            else
                write(buf, '\\')
                write(buf, not_eof_2(nc))
            end
        else
            write(buf, not_eof_2(c))
        end
        c = Lexer.readchar(ts)
        continue
    end
    return Expr(:macrocall, symbol("@cmd"), bytestring(buf))
end

function parse_interpolate(ps::ParseState, ts::TokenStream)
    c = Lexer.peekchar(ts)
    if Lexer.is_identifier_char(c)
        return parse_atom(ps, ts)
    elseif c === '('
        Lexer.readchar(ts)
        ex = parse_eqs(ps, ts)
        require_token(ps, ts) === ')' || throw(ParseError("invalid interpolation syntax"))
        take_token(ts)
        return ex
    else
        throw(ParseError("invalid interpolation syntax: \"$c\""))
    end
end

function tostr(buf::IOBuffer, custom::Bool)
    str = bytestring(buf)
    custom && return str
    str = unescape_string(str)
    !is_valid_utf8(str) && throw(ParseError("invalid UTF-8 sequence"))
    return str
end

function _parse_string_literal(ps::ParseState, ts::TokenStream, head::Symbol, n::Integer, custom::Bool)
    c  = Lexer.readchar(ts)
    b  = IOBuffer()
    ex = Expr(head)
    quotes = 0
    while true
        if c == '"'
            if quotes < n
                c = Lexer.readchar(ts)
                quotes += 1
                continue
            end
            push!(ex.args, tostr(b, custom))
            return ex
        elseif quotes == 1
            custom || write(b, '\\')
            write(b, '"')
            quotes = 0
            continue
        elseif quotes == 2
            custom || write(b, '\\')
            write(b, '"')
            custom || write(b, '\\')
            write(b, '"')
            quotes = 0
            continue
        elseif c === '\\'
            nxch = not_eof_3(Lexer.readchar(ts))
            if !custom || nxch !== '"'
                write(b, '\\')
            end
            write(b, nxch)
            c = Lexer.readchar(ts)
            quotes = 0
            continue
        elseif c === '$' && !custom
            iex = parse_interpolate(ps, ts)
            push!(ex.args, tostr(b, custom))
            push!(ex.args, iex)
            c = Lexer.readchar(ts)
            b = IOBuffer()
            quotes = 0
            continue
        else
            write(b, not_eof_3(c))
            c = Lexer.readchar(ts)
            quotes = 0
            continue
        end
    end
end

interpolate_string_literal(ex) = isa(ex, Expr) && length(ex.args) > 1
triplequote_string_literal(ex) = isa(ex, Expr) && ex.head === :triple_quoted_string

function parse_string_literal(ps::ParseState, ts::TokenStream, custom)
    if Lexer.peekchar(ts)  === '"'
        Lexer.takechar(ts)
        if Lexer.peekchar(ts) === '"'
            Lexer.takechar(ts)
            return _parse_string_literal(ps, ts, :triple_quoted_string, 2, custom)
        end
        return Expr(:single_quoted_string, "")
    end
    return _parse_string_literal(ps, ts, :single_quoted_string, 0, custom)
end

function _parse_atom(ps::ParseState, ts::TokenStream)
    t = require_token(ps, ts)
    if !isa(t, Char) && isa(t, Number)
        return take_token(ts)

    # char literal
    elseif t === symbol("'")
        take_token(ts)
        fch = Lexer.readchar(ts)
        fch === '\'' && ParseError("invalid character literal")
        if fch !== '\\' && !Lexer.eof(fch) && Lexer.peekchar(ts) === '\''
            # easy case 1 char no \
            Lexer.takechar(ts)
            return fch
        else
            c, b = fch, IOBuffer()
            while true
                c === '\'' && break
                write(b, not_eof_1(c))
                c === '\\' && write(b, not_eof_1(Lexer.readchar(ts)))
                c = Lexer.readchar(ts)
                continue
            end
            str = unescape_string(bytestring(b))
            if length(str) == 1
                # one byte e.g. '\xff' maybe not valid UTF-8
                # but we want to use the raw value as a codepoint in this case
                return str[1]
            else
                if length(str) != 1  || !is_valid_utf8(str)
                    ParseError("invalid character literal, got \'$str\'")
                end
                return str[1]
            end
        end

    # symbol / expression quote
    elseif t === :(:)
        take_token(ts)
        nt = peek_token(ps, ts)
        if is_closing_token(ps, nt) && (ps.space_sensitive || !isa(nt, Symbol))
            return :(:)
        end
        return Expr(:quote, _parse_atom(ps, ts))

    # misplaced =
    elseif t === :(=)
        throw(ParseError("unexpected \"=\""))

    # identifier
    elseif isa(t, Symbol)
        return take_token(ts)

    # parens or tuple
    elseif t === '('
        take_token(ts)
        @with_normal_ops ps begin
            @with_whitespace_newline ps begin
                if require_token(ps, ts) === ')'
                    # empty tuple
                    take_token(ts)
                    return Expr(:tuple)
                elseif peek_token(ps, ts) in Lexer.syntactic_ops
                    # allow (=) etc.
                    t = take_token(ts)
                    require_token(ps, ts) !== ')' && throw(ParseError("invalid identifier name \"$t\""))
                    take_token(ts)
                    return t
                else
                    # here we parse the first subexpression separately,
                    # so we can look for a comma to see if it is a tuple
                    # this lets us distinguish (x) from (x,)
                    ex = parse_eqs(ps, ts)
                    t  = require_token(ps, ts)
                    if t === ')'
                        take_token(ts)
                        if isa(ex, Expr) && ex.head === :(...)
                            # (ex...)
                            return Expr(:tuple, ex)
                        else
                            # value in parens (x)
                            return ex
                        end
                    elseif t === ','
                        # tuple (x,) (x,y) (x...) etc
                        return parse_tuple(ps, ts, ex)
                    elseif t === ';'
                        #parenthesized block (a;b;c)
                        take_token(ts)
                        if require_token(ps, ts) === ')'
                            # (ex;)
                            take_token(ts)
                            return Expr(:block, ex)
                        else
                            blk = parse_stmts_within_expr(ps, ts)
                            tok = require_token(ps, ts)
                            if tok === ','
                                throw(ParseError("unexpected comma in statment block"))
                            elseif tok != ')'
                                throw(ParseError("missing separator in statement block"))
                            end
                            take_token(ts)
                            return Expr(:block, ex, blk)
                        end
                    elseif t === ']' || t === '}'
                        throw(ParseError("unexpected \"$t\" in tuple"))
                    else
                        throw(ParseError("missing separator in tuple"))
                    end
                end
            end
        end

    # cell expression
    elseif t === '{'
        take_token(ts)
        if require_token(ps, ts) === '}'
            take_token(ts)
            return Expr(:cell1d)
        end
        vex = parse_cat(ps, ts, '}')
        if isempty(vex.args)
            return Expr(:cell1d)
        elseif vex.head === :comprehension
            ex = Expr(:typed_comprehension, TopNode(:Any))
            append!(ex.args, vex.args)
            return ex
        elseif vex.head === :dict_comprehension
            ex = Expr(:typed_dict_comprehension, Expr(:(=>), TopNode(:Any), TopNode(:Any)))
            append!(ex.args, vex.args)
            return ex
        elseif vex.head === :dict
            ex = Expr(:typed_dict, Expr(:(=>), TopNode(:Any), TopNode(:Any)))
            append!(ex.args, vex.args)
            return ex
        elseif vex.head === :hcat
            ex = Expr(:cell2d, 1, length(vex.args))
            append!(ex.args, vex.args)
            return ex
        else # Expr(:vcat, ...)
            nr = length(vex.args)
            if isa(vex.args[1], Expr) && vex.args[1].head === :row
                nc = length(vex.args[1].args)
                ex = Expr(:cell2d, nr, nc)
                for i = 2:nr
                    row = vex.args[i]
                    if !(isa(row, Expr) && row.head === :row && length(row.args) == nc)
                        throw(ParseError("inconsistent shape in cell expression"))
                    end
                end
                # Transpose to storage order
                sizehint!(ex.args, nr * nc + 2)
                for c = 1:nc, r = 1:nr
                    push!(ex.args, vex.args[r].args[c])
                end
                return ex
            else
                for i = 2:nr
                    row = vex.args[i]
                    if isa(row, Expr) && row.head === :row
                        throw(ParseError("inconsistent shape in cell expression"))
                    end
                end
                ex = Expr(:cell1d); ex.args = vex.args
                return ex
            end
        end

    # cat expression
    elseif t === '['
        take_token(ts)
        vex = parse_cat(ps, ts, ']')
        return vex

    # string literal
    elseif t === '"'
        take_token(ts)
        sl = parse_string_literal(ps, ts, false)
        if triplequote_string_literal(sl)
            return Expr(:macrocall, symbol("@mstr"), sl.args...)
        elseif interpolate_string_literal(sl)
            notzerolen = (s) -> !(isa(s, AbstractString) && isempty(s))
            return Expr(:string, filter(notzerolen, sl.args)...)
        end
        return sl.args[1]

    # macro call
    elseif t === '@'
        take_token(ts)
        @space_sensitive ps begin
            head = parse_unary_prefix(ps, ts)
            if (peek_token(ps, ts); ts.isspace)
                ex = Expr(:macrocall, macroify_name(head))
                append!(ex.args, parse_space_separated_exprs(ps, ts))
                return ex
            else
                call = parse_call_chain(ps, ts, head, true)
                if isa(call, Expr) && call.head === :call
                    nargs = length(call.args)
                    ex = Expr(:macrocall, macroify_name(call.args[1]))
                    sizehint!(ex.args, nargs)
                    for i = 2:nargs
                        push!(ex.args, call.args[i])
                    end
                    return ex
                else
                    ex = Expr(:macrocall, macroify_name(call))
                    append!(ex.args, parse_space_separated_exprs(ps, ts))
                    return ex
                end
            end
        end

    # command syntax
    elseif t === '`'
        take_token(ts)
        return parse_backquote(ps, ts)

    else
        throw(ParseError("invalid syntax: \"$(take_token(ts))\""))
    end
end

function parse_atom(ps::ParseState, ts::TokenStream)
    ex = _parse_atom(ps, ts)
    if (ex !== :(=>) && (ex in Lexer.syntactic_ops)) || ex === :(...)
        throw(ParseError("invalid identifier name \"$ex\""))
    end
    return ex
end

function is_valid_modref(ex::Expr)
    return length(ex.args) == 2 &&
           ex.head === :(.) &&
           ((isa(ex.args[2], Expr) &&
             ex.args[2].head === :quote &&
             isa(ex.args[2].args[1], Symbol)) ||
            (isa(ex.args[2], QuoteNode) &&
             isa(ex.args[2].value, Symbol))) &&
           (isa(ex.args[1], Symbol) || is_valid_modref(ex.args[1]))
end

function macroify_name(ex)
    if isa(ex, Symbol)
        return symbol(string('@', ex))
    elseif is_valid_modref(ex)
        return Expr(:(.), ex.args[1], Expr(:quote, macroify_name(ex.args[2].args[1])))
    else
        throw(ParseError("invalid macro use \"@($ex)"))
    end
end

#========================#
# Parser Entry Method
#========================#

function parse(ts::TokenStream)
    Lexer.skipws_and_comments(ts)
    t = Lexer.next_token(ts, false)
    while true
        Lexer.eof(t) && return nothing
        if Lexer.isnewline(t)
            t = Lexer.next_token(ts, false)
            continue
        end
        break
    end
    put_back!(ts, t)
    ps = ParseState()
    return parse_stmts(ps, ts)
end

parse(io::IO) = parse(TokenStream(io))
parse(str::AbstractString)  = parse(TokenStream(IOBuffer(str)))

end
