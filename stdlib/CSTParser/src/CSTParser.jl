module CSTParser
global debug = true

using Tokenize
import Base: length, first, last, getindex, setindex!
import Tokenize.Tokens
import Tokenize.Tokens: RawToken, AbstractToken, iskeyword, isliteral, isoperator, untokenize
import Tokenize.Lexers: Lexer, peekchar, iswhitespace

export ParseState, parse_expression

include("lexer.jl")
include("spec.jl")
include("utils.jl")
include("recovery.jl")
include("components/internals.jl")
include("components/keywords.jl")
include("components/lists.jl")
include("components/operators.jl")
include("components/strings.jl")
include("conversion.jl")
include("display.jl")
include("interface.jl")


"""
    parse_expression(ps)

Parses an expression until `closer(ps) == true`. Expects to enter the
`ParseState` the token before the the beginning of the expression and ends
on the last token.

Acceptable starting tokens are:
+ A keyword
+ An opening parentheses or brace.
+ An operator.
+ An instance (e.g. identifier, number, etc.)
+ An `@`.

"""
@addctx :expr function parse_expression(ps::ParseState)
    if ps.nt.kind == Tokens.COMMA
        push!(ps.errors, Error((ps.nt.startbyte:ps.nws.endbyte) .+ 1, "Expression began with a comma."))
        ret = ErrorToken(PUNCTUATION(next(ps)))
    elseif ps.nt.kind ∈ term_c && ps.nt.kind != Tokens.END
        push!(ps.errors, Error((ps.nt.startbyte:ps.nws.endbyte) .+ 1, "Expression began with a terminal token: $(ps.nt.kind)."))
        ret = ErrorToken(INSTANCE(next(ps)))
    else
        next(ps)
        if iskeyword(ps.t.kind) && ps.t.kind != Tokens.DO
            ret = parse_kw(ps)
        elseif ps.t.kind == Tokens.LPAREN
            ret = parse_paren(ps)
        elseif ps.t.kind == Tokens.LSQUARE
            ret = @default ps parse_array(ps)
        elseif ps.t.kind == Tokens.LBRACE
            ret = @default ps @closebrace ps parse_braces(ps)
        elseif isinstance(ps.t) || isoperator(ps.t)
            if ps.t.kind == Tokens.WHERE
                ret = IDENTIFIER(ps)
            else
                ret = INSTANCE(ps)
            end
            if is_colon(ret) && ps.nt.kind != Tokens.COMMA
                ret = parse_unary(ps, ret)
            end
        elseif ps.t.kind == Tokens.AT_SIGN
            ret = parse_macrocall(ps)
        else
            ret = ErrorToken(INSTANCE(ps))
            push!(ps.errors, Error((ps.nt.startbyte:ps.nws.endbyte) .+ 1, "Expression began with a : $(ps.nt.kind)."))
        end

        while !closer(ps)
            ret = parse_compound(ps, ret)
        end
    end
    return ret
end

function parse_compound(ps::ParseState, @nospecialize ret)
    if ps.nt.kind == Tokens.FOR
        ret = parse_generator(ps, ret)
    elseif ps.nt.kind == Tokens.DO
        ret = @default ps @closer ps block parse_do(ps, ret)
    elseif isajuxtaposition(ps, ret)
        op = OPERATOR(0, 0, Tokens.STAR, false)
        ret = parse_operator(ps, ret, op)
    elseif (ret isa EXPR{x_Str} ||  ret isa EXPR{x_Cmd}) && ps.nt.kind == Tokens.IDENTIFIER
        arg = IDENTIFIER(next(ps))
        push!(ret, LITERAL(arg.fullspan, arg.span, val(ps.t, ps), Tokens.STRING))
    elseif (ret isa IDENTIFIER || (ret isa BinarySyntaxOpCall && is_dot(ret.op))) && (ps.nt.kind == Tokens.STRING || ps.nt.kind == Tokens.TRIPLE_STRING || ps.nt.kind == Tokens.CMD)
        next(ps)
        arg = parse_string_or_cmd(ps, ret)
        head = arg.kind == Tokens.CMD ? x_Cmd : x_Str
        ret = EXPR{head}(Any[ret, arg])
    elseif ps.nt.kind == Tokens.LPAREN
        no_ws = !isemptyws(ps.ws)
        err_rng = ps.t.endbyte + 2:ps.nt.startbyte 
        ret = @closeparen ps parse_call(ps, ret)
        if no_ws && !(ret isa UnaryOpCall || ret isa UnarySyntaxOpCall)
            push!(ps.errors, Error(err_rng, "White space in function call."))
            ret = ErrorToken(ret)
        end
    elseif ps.nt.kind == Tokens.LBRACE
        if isemptyws(ps.ws)
            ret = @default ps @nocloser ps inwhere @closebrace ps parse_curly(ps, ret)
        else
            push!(ps.errors, Error(ps.t.endbyte + 2:ps.nt.startbyte , "White space in brace call."))
            ret = ErrorToken(@default ps @nocloser ps inwhere @closebrace ps parse_curly(ps, ret))
        end
    elseif ps.nt.kind == Tokens.LSQUARE && isemptyws(ps.ws) && !(ret isa OPERATOR)
        ret = @default ps @nocloser ps block parse_ref(ps, ret)
    elseif ps.nt.kind == Tokens.COMMA
        ret = parse_tuple(ps, ret)
    elseif isunaryop(ret) && ps.nt.kind != Tokens.EQ
        ret = parse_unary(ps, ret)
    elseif isoperator(ps.nt)
        op = OPERATOR(next(ps))
        ret = parse_operator(ps, ret, op)
    elseif ret isa UnarySyntaxOpCall && is_prime(ret.arg2)
        # prime operator followed by an identifier has an implicit multiplication
        nextarg = @precedence ps 11 parse_expression(ps)
        ret = BinaryOpCall(ret, OPERATOR(0, 0, Tokens.STAR,false), nextarg)
################################################################################
# Everything below here is an error
################################################################################
    elseif ps.nt.kind in (Tokens.RPAREN, Tokens.RSQUARE, Tokens.RBRACE)
        push!(ps.errors, Error((ps.t.startbyte:ps.nt.endbyte) .+ 1 , "Disallowed compound expression."))
        ret = EXPR{ErrorToken}([ret, ErrorToken(PUNCTUATION(next(ps)))])
    else
        push!(ps.errors, Error((ps.t.startbyte:ps.nt.endbyte) .+ 1 , "Disallowed compound expression."))
        nextarg = parse_expression(ps)
        ret = EXPR{ErrorToken}([ret, nextarg])
    end
    return ret
end

"""
    parse_paren(ps, ret)

Parses an expression starting with a `(`.
"""
@addctx :paren function parse_paren(ps::ParseState)  
    args = Any[PUNCTUATION(ps)]
    @closeparen ps @default ps @nocloser ps inwhere parse_comma_sep(ps, args, false, true, true)

    if length(args) == 2 && ((ps.ws.kind != SemiColonWS || (length(args) == 2 && args[2] isa EXPR{Block})) && !(args[2] isa EXPR{Parameters}))
        accept_rparen(ps, args)
        ret = EXPR{InvisBrackets}(args)
    else
        accept_rparen(ps, args)
        ret = EXPR{TupleH}(args)
    end
    return ret
end

"""
    parse(str, cont = false)

Parses the passed string. If `cont` is true then will continue parsing until the end of the string returning the resulting expressions in a TOPLEVEL block.
"""
function parse(str::String, cont = false)
    ps = ParseState(str)
    x, ps = parse(ps, cont)
    return x
end

function parse_doc(ps::ParseState)
    if (ps.nt.kind == Tokens.STRING || ps.nt.kind == Tokens.TRIPLE_STRING) && !isemptyws(ps.nws)
        doc = LITERAL(next(ps))
        if (ps.nt.kind == Tokens.ENDMARKER || ps.nt.kind == Tokens.END)
            return doc
        elseif isbinaryop(ps.nt) && !closer(ps)
            ret = parse_compound(ps, doc)
            return ret
        end

        ret = parse_expression(ps)
        ret = EXPR{MacroCall}(Any[GlobalRefDOC, doc, ret])
    elseif ps.nt.kind == Tokens.IDENTIFIER && val(ps.nt, ps) == "doc" && (ps.nnt.kind == Tokens.STRING || ps.nnt.kind == Tokens.TRIPLE_STRING)
        doc = IDENTIFIER(next(ps))
        next(ps)
        arg = parse_string_or_cmd(ps, doc)
        doc = EXPR{x_Str}(Any[doc, arg])
        ret = parse_expression(ps)
        ret = EXPR{MacroCall}(Any[GlobalRefDOC, doc, ret])
    else
        ret = parse_expression(ps)
    end
    return ret
end

function parse(ps::ParseState, cont = false)
    if ps.l.io.size == 0
        return (cont ? EXPR{FileH}(Any[]) : nothing), ps
    end
    last_line = 0
    curr_line = 0

    if cont
        top = EXPR{FileH}(Any[])
        if ps.nt.kind == Tokens.WHITESPACE || ps.nt.kind == Tokens.COMMENT
            next(ps)
            push!(top, LITERAL(ps.nt.startbyte, ps.nt.startbyte, "", Tokens.NOTHING))
        end

        while !ps.done && !ps.errored
            curr_line = ps.nt.startpos[1]
            ret = parse_doc(ps)

            # join semicolon sep items
            if curr_line == last_line && last(top.args) isa EXPR{TopLevel}
                push!(last(top.args), ret)
            elseif ps.ws.kind == SemiColonWS
                push!(top, EXPR{TopLevel}(Any[ret]))
            else
                push!(top, ret)
            end
            last_line = curr_line
        end
    else
        if ps.nt.kind == Tokens.WHITESPACE || ps.nt.kind == Tokens.COMMENT
            next(ps)
            top = LITERAL(ps.nt.startbyte, ps.nt.startbyte, "", Tokens.NOTHING)
        else
            top = parse_doc(ps)
            last_line = ps.nt.startpos[1]
            if ps.ws.kind == SemiColonWS
                top = EXPR{TopLevel}(Any[top])
                while ps.ws.kind == SemiColonWS && ps.nt.startpos[1] == last_line && ps.nt.kind != Tokens.ENDMARKER
                    ret = parse_doc(ps)
                    push!(top, ret)
                    last_line = ps.nt.startpos[1]
                end
            end
        end
    end

    return top, ps
end


function parse_file(path::String)
    x = parse(read(path, String), true)
    File([], [], path, x, [])
end

function parse_directory(path::String, proj = Project(path, []))
    for f in readdir(path)
        if isfile(joinpath(path, f)) && endswith(f, ".jl")
            try
                push!(proj.files, parse_file(joinpath(path, f)))
            catch
                println("$f failed to parse")
            end
        elseif isdir(joinpath(path, f))
            parse_directory(joinpath(path, f), proj)
        end
    end
    proj
end
end
